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

package de.tuberlin.uebb.sl2.modules

import scala.language.implicitConversions
import scala.language.postfixOps

/**
  * Enriched Lambda Calculus.
  */
trait EnrichedLambdaCalculus {

  self: Syntax with Errors with Type =>

  /**
    * The enriched lambda calculus.
    *
    * The SL abstract syntax will be translated into this core
    * representation for type checking and type inference.
    */
  sealed abstract class ELC {
    override def toString : String = ELCPrettyPrinter.pretty(this)
  }
  case class EInt(value: Int, attribute: Attribute = EmptyAttribute) extends ELC
  case class EChar(value: Char, attribute: Attribute = EmptyAttribute) extends ELC
  case class EStr(value: String, attribute: Attribute = EmptyAttribute) extends ELC
  case class ECon(con: ConVar, attribute: Attribute = EmptyAttribute) extends ELC
  case class EVar(ide: Var, attribute: Attribute = EmptyAttribute) extends ELC
  case class EApp(fun: ELC, expr: ELC, attribute: Attribute = EmptyAttribute) extends ELC
  case class ELam(pattern: EPattern, body: ELC, attribute: Attribute = EmptyAttribute) extends ELC
  case class ELet(definition: EDefinition, body: ELC, attribute: Attribute = EmptyAttribute) extends ELC
  case class ELetRec(definition: List[EDefinition], body: ELC, attribute: Attribute = EmptyAttribute) extends ELC
  case class EChoice(choices: List[ELC], attribute: Attribute = EmptyAttribute) extends ELC
  case class ECase(expr: ELC, alternatives: List[EAlternative], attribute: Attribute = EmptyAttribute) extends ELC

  /**
    * Local definitions.
    */
  sealed case class EDefinition(lhs: Var, sig: Option[Type], rhs: ELC, attribute: Attribute = EmptyAttribute)

  /**
    * Alternatives.
    */
  sealed case class EAlternative(pattern: EPattern, expr: ELC, attribute: Attribute = EmptyAttribute)

  /**
    * Patterns. 
    */
  sealed abstract class EPattern {

    /**
      * Extract all variables from pattern.
      */
    def vars: List[Var] = this match {
      case EPatternVar(v, _)       => List(v)
      case EPatternApp(_, pats, _) => pats.map(_.vars).flatten.distinct
    }
  }
  case class EPatternVar(ide: Var, attribute: Attribute = EmptyAttribute) extends EPattern
  case class EPatternApp(con: ConVar, conParams: List[EPattern], attribute: Attribute = EmptyAttribute) extends EPattern


  /**
    * Select the attribute of an ELC expression.
    */
  def attribute(e: ELC): Attribute = e match {
    case EInt(_, attr)       => attr
    case EChar(_, attr)      => attr
    case EStr(_, attr)       => attr
    case ECon(_, attr)       => attr
    case EVar(_, attr)       => attr
    case EApp(_, _, attr)    => attr
    case ELam(_, _, attr)    => attr
    case ELet(_, _, attr)    => attr
    case ELetRec(_, _, attr) => attr
    case EChoice(_, attr)    => attr
    case ECase(_, _, attr)   => attr
  }


  /**
    * Translate all top-level function definitions into the enriched lambda calculus.
    *
    * The transformation yields a representation of the program which is more suitable
    * for the type checker, where
    * $ - Pattern matching in top-level definitions is translated into a CHOICE expression,
    * $ - Conditionals are transformed  into a CASE expression,
    * $ - Lambda abstractions over multiple patterns are translated into nested unary
    *     lambda abstractions, and
    * $ - All top-level function definitions form the definitions of a recursive
    *     let-binding with the main-functions as its body.
    * 
    * For example, the following program
    * {{{
    * DEF isZero n = IF n == 0 THEN True ELSE False
    *
    * DEF add x y = x + y
    *
    * DEF empty Nil         = True
    * DEF empty (Cons x xs) = False
    *
    * DEF main = isZero 23
    * }}}
    * will be translated into the ELC-expression
    * {{{
    * LETREC isZero = \ n. CASE n == 0
    *                        OF True  -> True
    *                        OF False -> False
    *        add    = \ x. \ y. x + y
    *        empty  = CHOICE
    *                   \ Nil. True
    *                   \ (Cons x xs). False
    * IN isZero 23
    * }}}
    *
    * @param sigs Signatures of top-level function definitions
    * @param funDefs Top-level function definitions
    */
  def programToELC(sigs: Map[Var, FunctionSig], funDefs: Map[Var, List[FunctionDef]]) = {

    /*
     * Transform a top-level function into a local definition of a let-binding.
     */
    def makeEDef(name: Var, funDefs: List[FunctionDef]) = funDefs match {
      /* Function with a single definition */
      case List(funDef) => {
        val rhs = translateDef(funDef)
        val ty = lookupSig(name)
        EDefinition(name, ty, rhs, attribute(rhs))
      }

      /*
       * Function with multiple definitions (use of pattern matching).
       * A Function definition using pattern matching will be transformed
       * into a choice expression, i.e.,
       *     DEF f Nil        = 23
       *     DEF f List(x,xs) = 42
       * is transformed to
       *     f = CHOICE
       *           \ Nil . 23
       *           \ List(x,xs) . 42
       */
      case _ => {
        val rhs = funDefs.map(translateDef)
        val ty = lookupSig(name)
        val attr = attribute(rhs.head) // TODO: combine the attributes of all defs
        EDefinition(name, ty, EChoice(rhs, attr), attr)
      }
    }

    /*
     * Translate a single definition of a top-level function into a lambda abstraction.
     */
    def translateDef(funDef: FunctionDef) = {
      makeELam(funDef.patterns, funDef.expr, funDef.attribute)
    }

    /*
     * Check if a type signature is given for a function and translate the
     * signature's abstract syntax into a type.
     */
    def lookupSig(name: Var): Option[Type] = {
      sigs.get(name).map { (sig: FunctionSig) => astToType(sig.typ) }
    }

    /*
     * Get the definition of the main function. If the program doesn't contain a
     * main function, i.e., the user is programming a library, we will use a dummy
     * function consisting just of the constant 1. 
     */
    val main = funDefs.getOrElse("main", List(FunctionDef(Nil, ConstInt(1)))).head.expr

    /*
     * Translate all function definitions into local definitions for a
     * recursive let binding and annotate these definitions with the types
     * from the function's signature
     */
    val localDefs = (funDefs - "main").toList map (makeEDef _ tupled)

    ELetRec(localDefs, exprToELC(main), EmptyAttribute)
  }


  /**
    * Translate an SL expression into the enriched lambda calculus.
    */
  def exprToELC(expr: Expr): ELC = expr match {
    case ExVar(ide, attr)         => EVar(ide, attr)
    case ExCon(con, attr)         => ECon(con, attr)
    case ConstInt(value, attr)    => EInt(value, attr)
    case ConstChar(value, attr)   => EChar(value, attr)
    case ConstString(value, attr) => EStr(value, attr)
    case Case(expr, alts, attr)   => ECase(exprToELC(expr), alts.map(altToEAlt), attr)
    case App(fun, expr, attr)     => EApp(exprToELC(fun), exprToELC(expr), attr)

    /*
     * Conditionals are transformed into a case expression, i.e.,
     *         IF c THEN e1 ELSE e2
     * translates to
     *         CASE c OF
     *           True  -> e1
     *           False -> e2
     */
    case Conditional(cond, thenE, elseE, attr) => {
      val condELC = exprToELC(cond)
      val thenELC = exprToELC(thenE)
      val elseELC = exprToELC(elseE)

      ECase(condELC, List(
        EAlternative(EPatternApp("True", Nil), thenELC, attribute(thenE)),
        EAlternative(EPatternApp("False", Nil), elseELC, attribute(elseE))), attr)
    }

    /*
     * Lambda abstractions over multiple patterns are translated into
     * nested unary lambda abstractions, i.e.,
     *           \ x y (Foo z). <expr>
     * translates to
     *           \ x. \ y. \ (Foo z). <expr>
     */
    case Lambda(pats, expr, attr) => makeELam(pats, expr, attr)

    /*
     * Since SL's abstract syntax doesn't distinguish between recursive and
     * non-recursive let-bindings, we must assume that some definitions might
     * be recursive and translate the local definitions into a recursive
     * let-binding.
     */
    case Let(defs, body, attr) => {
      val elcBody = exprToELC(body)
      val elcDefs = defs.map(defToEDef)
      ELetRec(elcDefs, elcBody, attr)
    }
  }


  /**
    * Translate alternatives in CASE expressions to ELC alternatives.
    */
  def altToEAlt(alt: Alternative): EAlternative = {
    val elcPat = patToEPat(alt.pattern)
    val elc = exprToELC(alt.expr)

    EAlternative(elcPat, elc, alt.attribute)
  }


  /**
    * Translate patterns in definitions to ELC patterns.
    */
  def patToEPat(pat: Pattern): EPattern = pat match {
    case PatternVar(ide, attr) => EPatternVar(ide, attr)
    case PatternExpr(con, patExprs, attr) => EPatternApp(con, patExprs.map(patToEPat), attr)
  }


  /**
    * Translate local definitions in a let-binding into ELC definitions.
    */
  def defToEDef(ldef: LetDef) = {
    EDefinition(ldef.lhs, None, exprToELC(ldef.rhs), ldef.attribute)
  }


  /**
    * Build a chain of nested lambda abstractions from a list of patterns.
    */
  private def makeELam(pats: List[Pattern], expr: Expr, attr: Attribute) = {
    val elc = exprToELC(expr)
    val buildLambda = (pat: Pattern, e: ELC) => ELam(patToEPat(pat), e, attr)

    pats.foldRight(elc)(buildLambda)
  }


  /**
    * Pretty printer for the enriched lambda calculus.
    */
  object ELCPrettyPrinter extends org.kiama.output.PrettyPrinter with Lexic {

    def pretty(expr: ELC): String = super.pretty(elcDoc(expr))

    implicit def elcDoc(expr: ELC): Doc = expr match {
      case EVar(ide, _)         => ide
      case ECon(con, _)         => con
      case EInt(num, _)         => value(num)
      case EChar(char, _)       => dquotes(value(char))
      case EStr(str, _)         => dquotes(value(str))
      case ELam(pat, e, _)      => parens(lambdaLex <+> pat <+> dotLex <> nest(line <> e))
      case ECase(e, alts, _)    => caseLex <+> e <@> ssep(alts map alternativeDoc, linebreak)
      case ELet(ldef, e, _)     => letLex <+> ldef <@> inLex <> nest(line <> e)
      case ELetRec(ldefs, e, _) => "LETREC" <+> nest(line <> cat(ldefs map definitionDoc)) <@> inLex <> nest(line <> e)
      case EApp(f, e, _)        => parens(f <+> e)
      case EChoice(choices, _)  => "CHOICE" <+> catList(choices map elcDoc, line)
    }

    implicit def definitionDoc(ldef: EDefinition): Doc = ldef.lhs <+> funEqLex <> nest(line <> ldef.rhs)

    implicit def alternativeDoc(alt: EAlternative): Doc = ofLex <+> alt.pattern <+> thenLex <+> alt.expr

    implicit def patternDoc(pat: EPattern): Doc = pat match {
      case EPatternVar(v, _)         => v
      case EPatternApp(con, pats, _) => con <+> catList(pats map patternDoc, "")
    }

    def catList(list: List[Doc], sep: Doc): Doc = group(nest(lsep(list, sep)))
  }

}
