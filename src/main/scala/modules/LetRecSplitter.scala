/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * * Neither the name of the TU Berlin nor the
 * names of its contributors may be used to endorse or promote products
 * derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

package de.tuberlin.uebb.sl2.modules

import de.tuberlin.uebb.sl2.modules.Syntax.{ VarFirstClass }
import scala.collection.immutable.List.{ fill }
import scala.util.Either.{ cond }

/**
 *
 */
trait LetRecSplitter {

  self: Syntax with EnrichedLambdaCalculus with Graph[VarFirstClass] with Errors =>

  /**
   * Letrec splitting
   *
   * This function performs a dependency analysis for each letrec-
   * expression and splits it into minimal let- and letrec-expressions.
   *
   * @param boundVars Variables bound in outer context
   * @param expr Expression to split
   */
  def splitLetRecs(boundVars: Set[VarFirstClass], expr: ELC): Either[Error, ELC] = expr match {
    case ELetRec(defs, body, attr) => {
      /*
	 * Build a dependency graph; the left-hand sides of the letrec are the
	 * vertices of the graph and edges `f' -> `g' are added whenever the
	 * definition of the left-hand side `f' requires the definition of the
	 * left-hand side `g'.
	 */
      for (
        gs <- buildDepGraph(boundVars, expr).right;

        /*
			 * Split the right-hand sides of the letrec's definitions which
			 * might contain letrecs themselves. Care has to be taken regarding
			 * the outer context. For each definition v = e only those left-hand
			 * sides of the letrec may be added to the outer context the expression
			 * e depends on, i.e., those v has a dependency edge to. Otherwise,
			 * expressions in which inner recursive functions shadow outer bindings
			 * are not split correctly, like:
			 *
			 * letrec f = \ x . x
			 * main = letrec f = \ x . case x <= 1 of
			 * True -> 1
			 * False -> f (x - 1)
			 * in f 3
			 * in main
			 *
			 * If we put {f, main} in the outer context when splitting the
			 * right-hand side of the `main' definition, the inner `f' is not
			 * recognized as recursive
			 */
        splitRhs <- {
          val (graph, succs) = gs

          def splitRhs(d: EDefinition) = {
            val visibleLhsVars = succs.get(d.lhs).get.toSet
            splitLetRecs(boundVars union visibleLhsVars, d.rhs)
          }

          errorMap(defs, splitRhs).right
        };

        /*
			 * Split the body. In contrast to the right-hand sides of the
			 * definitions we split the body with all left-hand sides in
			 * the outer context, since they are all truly visible in the body.
			 */
        splitBody <- {
          val lhsVars = defs.map(_.lhs).toSet
          splitLetRecs(boundVars union lhsVars, body).right
        }
      ) yield {
        val (graph, succs) = gs
        // Perform the actual splitting
        val leftHandSides = defs map (_.lhs)
        val signatures = leftHandSides.zip(defs.map(_.sig)).toMap
        val splitDefs = leftHandSides.zip(splitRhs).toMap
        val sccs = topologicalSort(stronglyConnectedComponents(graph), graph)

        /*
		 * Split the definition(s) in the strongly connected component.
		 * Non-recursive functions with no dependencies will be transformed
		 * into a simple, non-recursive let-binding. Otherwise, we generate
		 * a recursive let-binding for each strongly connected component.
		 */
        def split(scc: Set[VarFirstClass], body: ELC) = {
          // A definition depends on no other function definitions
          if (scc.size == 1) {
            val lhs = scc.head
            val lhsSuccessors = succs.get(lhs).get
            val definition = EDefinition(lhs, signatures.get(lhs).get, splitDefs.get(lhs).get, attr)
            // Recursive function in defintion
            if (lhsSuccessors contains lhs) ELetRec(List(definition), body, attr)
            // Non-recursive function in defintion
            else ELet(definition, body, attr)
          } // A definition depends on other function definitions
          else {
            val definitions: List[EDefinition] = scc.map { lhs => EDefinition(lhs, signatures.get(lhs).get, splitDefs.get(lhs).get, attr) }.toList
            ELetRec(definitions, body, attr)
          }
        }

        sccs.foldRight(splitBody)(split)
      }
    }

    /*
 * The following cases are syntax traversal splitting each
 * subexpression. Lexical scoping of non-recursive let-,
 * lambda-, and case-expressions is respected by adding the
 * bound variables to 'splitLetRecs' first argument.
 */
    case EApp(fun, arg, attr) => {
      for (
        splitFun <- splitLetRecs(boundVars, fun).right;
        splitArg <- splitLetRecs(boundVars, arg).right
      ) yield EApp(splitFun, splitArg, attr)
    }

    case ELam(pattern, body, attr) => {
      for (splitBody <- splitLetRecs(boundVars union pattern.vars.map(Syntax.Var(_)).toSet, body).right)
        yield ELam(pattern, splitBody, attr)
    }

    case ELet(EDefinition(lhs, sig, rhs, attrD), body, attrL) => {
      for (
        splitRhs <- splitLetRecs(boundVars, rhs).right;
        splitBody <- splitLetRecs(boundVars + lhs, body).right
      ) yield ELet(EDefinition(lhs, sig, splitRhs, attrD), splitBody, attrL)
    }

    case EChoice(choices, attr) => {
      for (splitChoices <- errorMap(choices, (c: ELC) => splitLetRecs(boundVars, c)).right)
        yield EChoice(splitChoices, attr)
    }

    case ECase(expr, alts, attr) => {

      def splitLetRecsAlt(alt: EAlternative) = {
        for (splitExpr <- splitLetRecs(boundVars union alt.pattern.vars.map(Syntax.Var(_)).toSet, alt.expr).right)
          yield EAlternative(alt.pattern, splitExpr, alt.attribute)
      }

      for (
        splitExpr <- splitLetRecs(boundVars, expr).right;
        splitAlts <- errorMap(alts, splitLetRecsAlt).right
      ) yield ECase(splitExpr, splitAlts, attr)
    }

    case _ => Right(expr)
  }

  type Successors = Map[VarFirstClass, List[VarFirstClass]]

  type DependencyGraph = (Graph, Successors)

  /**
   * Build dependency graph.
   *
   * This method detects undefined variables in the given expression.
   */
  def buildDepGraph(outerCtx: Set[VarFirstClass], expr: ELC): Either[Error, DependencyGraph] = {

    var succs: Successors = Map.empty

    /**
     * Record a dependency edge.
     */
    def recordEdge(from: VarFirstClass, to: VarFirstClass) = succs.get(from) match {
      case None => succs = succs + (from -> List(to))
      case Some(vs) => succs = succs + (from -> (to :: vs))
    }

    /**
     * Traverse a definition.
     *
     * @param outerCtx Outer context
     * @param lhs Left-hand sides of current letrec
     * @param d Definition to look at
     */
    def traverseDef(outerCtx: Set[VarFirstClass], lhs: Set[VarFirstClass])(d: EDefinition): Either[Error, Unit] = {
      traverseRhs(d.lhs, lhs, outerCtx, d.rhs)
    }

    /**
     * Traverse the right-hand side of a definition.
     *
     * @param f Name of the current definition (source of vertices added, if any)
     * @param lhs Left-hand sides bound in current letrec
     * @param outerCtx Names available in outer context
     */
    def traverseRhs(f: VarFirstClass, lhs: Set[VarFirstClass], outerCtx: Set[VarFirstClass], expr: ELC): Either[Error, Unit] = expr match {
      case EVar(v, attr) => cond(isKnown(v)(lhs, outerCtx),
        // Only do something if v is bound in the current letrec
        if (isDep(v)(lhs, outerCtx)) recordEdge(f, v),
        UndefinedError("variable", v.toString, attr))

      case EApp(fun, arg, _) => collectErrors(traverseRhs(f, lhs, outerCtx, fun),
        traverseRhs(f, lhs, outerCtx, arg))

      case ELam(pattern, body, _) => {
        // Body has to be traversed with pattern variables in outer context
        val patVars = pattern.vars.map(Syntax.Var(_).asInstanceOf[VarFirstClass]).toSet
        traverseRhs(f, lhs, outerCtx union patVars, body)
      }

      case ELet(EDefinition(v, _, rhs, _), body, _) => {
        // Similar to lambda-abstraction
        collectErrors(traverseRhs(f, lhs, outerCtx, rhs),
          traverseRhs(f, lhs, outerCtx + v, body))
      }

      case ELetRec(defs, body, _) => {
        /*
		 * Right-hand sides are traversed with all left-hand sides
		 * in the outer context. This realizes that bindings in inner
		 * letrecs shadow bindings in outer letrecs.
		 */
        val knownVars = outerCtx union (defs map (_.lhs)).toSet
        val rightHandSides = defs map (_.rhs)

        for (
          _ <- errorMap(rightHandSides, (e: ELC) => traverseRhs(f, lhs, knownVars, e)).right;
          _ <- traverseRhs(f, lhs, knownVars, body).right
        ) yield ()
      }

      case EChoice(choices, _) => {
        for (_ <- errorMap(choices, (e: ELC) => traverseRhs(f, lhs, outerCtx, e)).right)
          yield ()
      }

      case ECase(e, alts, _) => {
        /*
	 * Traverse each branch of the case expression like a
	 * lambda-abstraction or a let-binding.
	 */
        def traverseAlt(alt: EAlternative) = {
          val patVars = alt.pattern.vars.map(Syntax.Var(_).asInstanceOf[VarFirstClass]).toSet
          traverseRhs(f, lhs, outerCtx union patVars, alt.expr)
        }

        for (
          _ <- traverseRhs(f, lhs, outerCtx, e).right;
          _ <- errorMap(alts, traverseAlt).right
        ) yield ()
      }

      case _ => Right()
    }

    /**
     * Variable 'v' is known in the left-hand sides of the current letrec
     * or is available in the outer context, i.e., 'v' is known at all.
     */
    def isKnown(v: VarFirstClass)(lhs: Set[VarFirstClass], outerCtx: Set[VarFirstClass]) = {
      (lhs contains v) || (outerCtx contains v)
    }

    /**
     * Variable 'v' is known in the left-hand sides of the current letrec and is
     * not in the outer context, i.e., a dependency edge has to be inserted for v.
     */
    def isDep(v: VarFirstClass)(lhs: Set[VarFirstClass], outerCtx: Set[VarFirstClass]) = {
      (lhs contains v) && (!outerCtx.contains(v))
    }

    expr match {
      case ELetRec(defs, _, _) => {
        val leftHandSides = defs map { (d: EDefinition) => d.lhs }

        /* Set up the graph with the left-hand sides as vertices and no edges. */
        succs = (leftHandSides map { (lhs: VarFirstClass) => (lhs -> Nil) }).toMap

        for (_ <- errorMap(defs, traverseDef(outerCtx, leftHandSides.toSet)).right) yield {
          val vertices = succs.keySet
          val edges = succs.toList.flatMap { case (from, tos) => fill(tos.length)(from) zip tos }

          (directedGraph(vertices, edges), succs)
        }
      }

      case _ => Left(GenericError("Dependency graph of non letrec requested"))
    }
  }

}

