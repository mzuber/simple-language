/*
 * Copyright (c) 2012, Technische Universität Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided
 *   with the distribution.
 * - Neither the name of the TU Berlin nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package de.tuberlin.uebb.sl2.impl

import de.tuberlin.uebb.sl2.modules._
import scala.util.Either.{cond}

/**
  * The SL type checker.
  *
  * This implementation basically performs Hindley-Milner type inference
  * for the enriched lambda calculus, providing additional means to check
  * the types given in a signature of a top level definition against the
  * inferred ones. It follows closely the presentation in Simon Peyton Jones'
  * "The Implementation of Functional Programming Languages" (1987).
  */
trait TypeCheckerImpl extends TypeChecker with Lexic with Syntax with EnrichedLambdaCalculus with Context with Type with NameSupply with Substitution with Unification with Errors {
  
  /**
    * Inferr the type for the expression with respect to the given context.
    */
  def checkTypes(ctx: Context, e: ELC) : Either[Error, Type] = inferType(ctx, e) match {
    case Left(error)    => Left(error)
    case Right((_, ty)) => Right(ty)
  }


  /**
    * Type inference on the enriched lambda calculus.
    * 
    * @param ctx A type environment, associating a type with each of the
    *            free variables of `e`.
    * @param e The expression to be checked.
    * @return If a type could be inferred successfully, the method returns a
    *         tuple (σ,τ), where σ is a substitution defined on the unknown
    *         type variables in the given context and τ is the type derived
    *         for `e`, such that σ(`ctx`) ⊢ `e` : τ.
    */
  def inferType(ctx: Context, e: ELC): Either[Error, (Substitution, Type)] = e match {

    /* Built-in values (Int, Char, String) */ 
    case EInt(_,_)  => Right(empty, BaseType(Integer))
    case EChar(_,_) => Right(empty, BaseType(Character))
    case EStr(_,_)  => Right(empty, BaseType(String))

    /* Variables and constructors */
    case EVar(ide, attr) => ctx.lookupFresh(ide) match {
      case Some(ty) => Right(empty, ty)
      case None     => Left(UndefinedError("identifier", ide, attr))
    }
    case ECon(con, attr) => ctx.lookupFresh(con) match {
      case Some(ty) => Right(empty, ty)
      case None     => Left(UndefinedError("constructor", con, attr))
    }

    /* Function application */
    case EApp(fun, expr, attr) => inferType(ctx, List(fun, expr)) match {
      case Left(error) => Left(error)
      case Right( (σ, List(s,t)) ) => {
	val tv: TypeVariable = freshTVar()
	unify(s :==: FunctionType(t, tv)) match {
	  case Left(unifyError) => Left(TypeError("application", attr, unifyError))
	  case Right(φ)         => {
	    val ψ = φ <+> σ
	    Right(ψ, ψ :@ tv)
	  }
	}
      }
    }

    /* Let-bindings */
    case ELet(EDefinition(lhs, sig, rhs, _), body, attr) => {
      for ( (σ, rhsType) <- inferType(ctx, rhs).right ;
	    φ <- {
	      /* 
	       * Check if the inferred type matches the type signature, i.e.,
	       * both types are unifiable and the given type ascription is not
	       * more general than the inferred one.
	       */
	      val equation = sig match {
	        case Some(sigType) => rhsType :>=: sigType
	        case None          => rhsType :==: freshTVar()
	      }
	      handleUnifyError(lhs, attr)(unify(equation)).right
	    } ;
	    (ψ, bodyType) <- {
	      /*
	       * Add the generalized type of the definition's right-hand
	       * side to the context and infer the body's type.
	       */
	      val typeScheme = sig.getOrElse(rhsType).generalize(σ(ctx))
	      val context = σ(ctx) + (lhs -> typeScheme)
	      inferType(context, body).right
	    } )
      yield (ψ <+> σ, bodyType)
    }

    /* Recursive let-bindings */
    case ELetRec(defs, body, attr) => {
      /*
       * Make up monomorphic types, i.e., a monomorphic variant of the
       * type signature if given or a fresh type variable, for all left-
       * hand side variables.
       */
      val (lhsCtx, sigTypes) = {
	val lhsSigTuples = defs map { (d:EDefinition) => (d.lhs, d.sig.getOrElse(freshTVar())) }
	(lhsSigTuples.toMap, lhsSigTuples.map(_._2))
      }

      /**
        * Generate the corresponding unification equation for each definition.
	*/
      val equationBuilder = {
	def chooseEquation(d: EDefinition) = d.sig match {
	  case None    => { (lhs: Type, rhs: Type) => lhs :==: rhs }
	  case Some(_) => { (lhs: Type, rhs: Type) => lhs :>=: rhs }
	}
	defs map chooseEquation
      }

      for ( (σ, rhsTypes) <- {
	      /*
	       * Calcualte types and a substitution of the right-hand sides
	       * with respect to the outer context and bindings given in 'lhsCtx'.
	       */
	      inferType(lhsCtx <++> ctx, defs map (_.rhs)).right
            } ;
	    (φ, lhsTypes) <- {
	      /*
	       * Unify the left-hand side and the right-hand side types.
	       */
	      val lhsTypes = σ(lhsCtx).lookupList(defs map (_.lhs)).get
	      val equations = equationBuilder(lhsTypes, rhsTypes)
	      handleUnifyError("letrec", attr)(unify(equations)).right.map((χ: Substitution) => (χ <+> σ, lhsTypes)).right
	    } ;
	    ψ <- {
	      /*
	       * Check if the inferred left-hand side types match the given type signatures.
	       */
	      val equations = equationBuilder(sigTypes, lhsTypes)
	      handleUnifyError("letrec", attr)(unify(equations)).right
	    } ;
	    (ω, bodyType) <- {
	      /*
	       * Make up the context to check the body of the letrec.
	       * Apply the substitution from the previous unification step
	       * and generalize the types of the left-hand side variables.
	       */	      
	      val context = φ(lhsCtx).mapValues(_.generalize(φ(ctx))) <++> φ(ctx)
	      inferType(context, body).right
	    } )
      yield (ω <+> φ, bodyType)
    }

    /* Lambda abstraction */
    case ELam(pat, body, attr) => {
      for ( (patCtx, argType) <- inferType(ctx, pat).right ;
	    (σ, bodyType) <- inferType(patCtx <++> ctx, body).right )
      yield (σ, FunctionType(σ :@ argType, bodyType))
    }

    /* Choice expressions */
    case EChoice(choices, attr) => {
      for ( (σ, types) <- inferType(ctx, choices).right ;
	    φ <- {
	      // all types of the different choice expressions must be equal
	      handleUnifyError("function alternatives", attr)(unify(allEqual(types))).right
	    } ;
	    (ψ, choiceType) <- {
	      val ψ = φ <+> σ
	      // all types of the different choice expressions must be function types
	      val wellFormedWitness = types.head
	      cond(wellFormedWitness.isInstanceOf[FunctionType],
		   (ψ, ψ :@ wellFormedWitness),
		   TypeError("choice", attr, GenericError("Alternatives of choice are not of function type"))).right
	    } )
      yield (ψ, choiceType)
    }

    /* Case expression */
    case ECase(expr, alts, attr) => {
      for ( (σ, exprType) <- inferType(ctx, expr).right ;
	    (rhsContexts, lhsTypes) <- {
	      val pats = alts.map(_.pattern)
              errorMap(pats, (p: EPattern) => inferType(ctx, p)).right.map(_.unzip).right
	    } ;
	    φ <- {
	      val equations = allEqual(exprType :: lhsTypes)
	      handleUnifyError("patterns of case expression", attr)(unify(equations)).right.map(_ <+> σ).right
	    } ;
	    (substitutions, rhsTypes) <- {
	      val rhs = alts.map(_.expr)
	      val ctxRhsList = rhsContexts.map(φ(_)).zip(rhs)
	      val infer = (c: Context, e: ELC) => inferType(c <++> φ(ctx), e)
	      errorMap(ctxRhsList, infer.tupled).right.map(_.unzip).right
	    } ;
	    ω <- {
	      val ψ = compose(substitutions :+ φ)
	      val equations = allEqual(rhsTypes.map(ψ :@ _))
	      handleUnifyError("alternatives of case expression", attr)(unify(equations)).right.map(_ <+> ψ).right
	    } )
      yield (ω, ω :@ rhsTypes.head)
    }
  }


  /**
    * Infer the types of a list of expressions and build a corresponding
    * substitution for the context.
    */
  def inferType(ctx: Context, exprs: List[ELC]): Either[Error, (Substitution, List[Type])] = exprs match {
    case Nil     => Right(empty, Nil)
    case e :: es => {
      for ( (σ, t)  <- inferType(ctx, e).right ;
	    (φ, ts) <- inferType(σ(ctx), es).right )
      yield {
	(φ <+> σ, (φ :@ t) :: ts)
      }
    }
  }


  /**
    * Type inference for patterns.
    *
    * Infer the type of a pattern, i.e. the type of a function's argument
    * (if in a lambda) or the type of the scrutinee (if in a case expression).
    * This method additionally returns a context with monomorphic types for
    * the pattern's variables.
    */
  def inferType(ctx: Context, pat: EPattern): Either[Error, (Context, Type)] = pat match {
    case EPatternVar(ide, _) => {
      val tv = freshTVar()
      Right(Map(ide -> tv), tv)
    }
    case EPatternApp(con, pats, attr) => ctx.lookupFresh(con) match {
      case None => Left(UndefinedError("constructor in pattern", con, attr))
      case Some(conTy) => {
	if (conTy.argTypes.length != pats.length) {
	  Left(TypeError("constructor in pattern", attr,
	       GenericError("constructor" + quote(con) + "not fully applied")))
	}
	else {
	  for ( (contexts, patTys) <- {
	          val inferPatType = (p: EPattern) => inferType(ctx, p)
                  errorMap(pats, inferPatType).right.map(_.unzip).right
                  } ;
		σ <- {
		  val equations = (patTys, conTy.argTypes).zipped.map(_ :==: _)
		  handleUnifyError("constructor application in pattern", attr)(unify(equations)).right
		} )
	  yield (join(contexts.map(σ(_))), σ :@ conTy.resType)
	}
      }
    }
  }


  /**
    * Generate a list of equations stating that all given types must be equal.
    */
  def allEqual(types: List[Type]): List[Equation] = (types, types.tail).zipped map (_ :==: _)


  /**
    * If a computation unifying two types fails, we
    * wrap the occurring error into a type check error.
    */
  def handleUnifyError[T](what: String, attr: Attribute)(res: Either[Error, T]) = res match {
    case Left(error) => Left(TypeError(what, attr, error))
    case Right(_)    => res
  }
}
