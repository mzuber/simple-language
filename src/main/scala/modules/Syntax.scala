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

/**
  * Abstract syntax of SL.
  */
trait Syntax {

  /**
    * Simple data structure representing the position (line and column) in a source file.
    */
  case class Position(line: Int, col: Int)

  /**
    * A location represents the area in a source file where a syntactical
    * element is defined. The empty location is useful when writing syntax trees
    * manually.
    */
  sealed abstract class Location
  case class FileLocation(file: String, from: Position, to: Position) extends Location
  case object NoLocation extends Location

  /**
    * Each node in the syntax tree carries a set of attributes around. Right
    * now, these are only source code locations. The empty attribute is useful
    * when writing syntax trees manually.
    */
  sealed abstract class Attribute {

    /*
     * This little hack allows us to compare AST elements without taking the
     * attributes into account. If attributes matter, we have to access them
     * explicitely
     */
    override def equals(other: Any) = other match {
      case a: Attribute => true
      case _            => false
    }

    override def hashCode(): Int = { classOf[Attribute].hashCode() }
  }
  case class AttributeImpl(location: Location) extends Attribute
  case object EmptyAttribute extends Attribute

  /*
   * All identifiers are currently represented by strings (since we do not use a symbol table).
   * 
   * TODO: Rename those types such that it's clear that they represent the types for certain kinds of identifiers.
   */
  type Var = String

  type TypeVar = String

  type ConVar = String

  type TConVar = String


  /**
    * Abstract syntax trees for SL.
    */
  sealed abstract class AST {
    override def toString: String = ASTPrettyPrinter.pretty(this)
  }

  /**
    * An SL program consists of top level definitions which might be
    * annotated with an explicit type signature.
    */
  case class Program(signatures: Map[Var, FunctionSig], functionDefs: Map[Var, List[FunctionDef]],
		     dataDefs: List[DataDef], attribute: Attribute = EmptyAttribute) extends AST

  /**
    * An empty program, i.e., a program with no signatures or top-level definitions.
    */
  val emptyProgram = Program(Map.empty, Map.empty, Nil)


  /**
    * Type signature for top-level function definitions.
    */
  case class FunctionSig(typ: ASTType, attribute: Attribute = EmptyAttribute)

  /**
    *  Top-level function definitions.
    */
  case class FunctionDef(patterns: List[Pattern], expr: Expr, attribute: Attribute = EmptyAttribute)

  /**
    * Patterns for top-level function definitions.
    */
  sealed abstract class Pattern {

    /**
      * A set of all variables occuring in this pattern.
      */
    def vars: Set[Var] = varsList.toSet

    /**
      * A list of all variables occuring in this pattern.
      */
    def varsList: List[Var] = this match {
      case PatternVar(x, _)        => List(x)
      case PatternExpr(_, pats, _) => pats.flatMap(_.vars)
    }
  }

  case class PatternVar(ide: Var, attribute: Attribute = EmptyAttribute) extends Pattern
  case class PatternExpr(con: ConVar, patExprs: List[Pattern], attribute: Attribute = EmptyAttribute) extends Pattern
  

  /**
    * Data type definitions.
    */
  case class DataDef(ide: TConVar, tvars: List[TypeVar], constructors: List[ConstructorDef], attribute: Attribute = EmptyAttribute)

  /**
    * A datatype consists of a list of data constructors CondDef and the types of the arguments they take.
    */
  case class ConstructorDef(constructor: ConVar, types: List[ASTType], attribute: Attribute = EmptyAttribute)

  /**
    * All type constructors of a program.
    */
  def allTypeCons(dataDefs: List[DataDef]): List[TConVar] = dataDefs map (_.ide)

  /**
    * All data constrcutor of a program.
    */
  def allDataCons(dataDefs: List[DataDef]): List[ConVar] = dataDefs.flatMap(_.constructors).map(_.constructor)

  /**
    * All applications of type constructors in type definitions.
    */
  def allAppTypeCons(conDefs: List[ConstructorDef]): List[TConVar] = {
    val conTypes: List[ASTType] = conDefs flatMap (_.types)

    def selectAppTypeCon(ty: ASTType) = ty match {
      case TyExpr(conType, _, _) => List(conType)
      case _                     => Nil
    }

    conTypes flatMap selectAppTypeCon
  }

  /**
    * Abstract syntax for types.
    */
  sealed abstract class ASTType {
    
    /**
      * Arity of this type.
      */
    def arity: Int = this match {
      case FunTy(types, _) => types.length - 1
      case _               => 0
    }
  }

  case class TyVar(ide: TypeVar, attribute: Attribute = EmptyAttribute) extends ASTType
  case class FunTy(types: List[ASTType], attribute: Attribute = EmptyAttribute) extends ASTType
  case class TyExpr(conType: TConVar, typeParams: List[ASTType], attribute: Attribute = EmptyAttribute) extends ASTType


  /**
    * Right-hand sides of top-level definitions consist of expressions.
    */
  sealed abstract class Expr {
    override def toString(): String = ASTPrettyPrinter.pretty(this)
  }
  case class Conditional(condition: Expr, thenE: Expr, elseE: Expr, attribute: Attribute = EmptyAttribute) extends Expr
  case class Lambda(patterns: List[Pattern], expr: Expr, attribute: Attribute = EmptyAttribute) extends Expr
  case class Case(expr: Expr, alternatives: List[Alternative], attribute: Attribute = EmptyAttribute) extends Expr
  case class Let(definitions: List[LetDef], body: Expr, attribute: Attribute = EmptyAttribute) extends Expr
  case class App(function: Expr, expr: Expr, attribute: Attribute = EmptyAttribute) extends Expr
  case class ExVar(ide: Var, attribute: Attribute = EmptyAttribute) extends Expr
  case class ExCon(con: ConVar, attribute: Attribute = EmptyAttribute) extends Expr
  case class ConstInt(value: Int, attribute: Attribute = EmptyAttribute) extends Expr
  case class ConstChar(value: Char, attribute: Attribute = EmptyAttribute) extends Expr
  case class ConstString(value: String, attribute: Attribute = EmptyAttribute) extends Expr

  /**
    * Local definition in a let-binding.
    */
  case class LetDef(lhs: Var, rhs: Expr, attribute: Attribute = EmptyAttribute)

  /**
    * Alternative in a case expression.
    */
  case class Alternative(pattern: Pattern, expr: Expr, attribute: Attribute = EmptyAttribute)

  /**
    * Select the attribute of an expression.
    */
  def attribute(expr: Expr): Attribute = expr match {
    case Conditional(_, _, _, attr) => attr
    case Lambda(_, _, attr)         => attr
    case Case(_, _, attr)           => attr
    case Let(_, _, attr)            => attr
    case App(_, _, attr)            => attr
    case ExVar(_, attr)             => attr
    case ExCon(_, attr)             => attr
    case ConstInt(_, attr)          => attr
    case ConstChar(_, attr)         => attr
    case ConstString(_, attr)       => attr
  }
  

  /**
    * Pretty printer for SL definitions and expressions.
    */
  object ASTPrettyPrinter extends org.kiama.output.PrettyPrinter with Lexic {

    def pretty(in: Any): String = in match {
      case expr: Expr    => super.pretty(exprDoc(expr))
      case prog: Program => super.pretty(programDoc(prog))
      case any           => pretty_any(any)
    }

    implicit def programDoc(prog: Program): Doc = {
      var doc = line

      for (dataDef <- prog.dataDefs)
        doc = doc <@> dataDef

      for ((name, sig) <- prog.signatures)
        doc = doc <@> funLex <+> name <+> typeLex <+> sig.typ <> line

      for ((name, funDefs) <- prog.functionDefs ;
	   funDef <- funDefs) {
	doc = doc <@> defLex <+> name <+> catList(funDef.patterns map patternDoc, "") <+> funEqLex <+> funDef.expr <> line
      }

      doc
    }

    implicit def dataDefDoc(dataDef : DataDef): Doc = dataDef match {
      case DataDef(name, Nil, cons, _) => {
        dataLex <+> name <+> funEqLex <+> catList(cons map constructorDoc, space <> dataSepLex) <> line
      }
      case DataDef(name, vars, cons, _) => {
        dataLex <+> name <+> hsep(vars map value) <+> funEqLex <+> catList(cons map constructorDoc, space <> dataSepLex) <> line
      }
    }

    implicit def constructorDoc(c: ConstructorDef) = c.constructor <+> hsep(c.types map typeDoc)

    implicit def typeDoc(ty: ASTType): Doc = ty match {
      case TyVar(ide, _)          => ide
      case TyExpr(cons, Nil, _)   => cons
      case TyExpr(cons, types, _) => parens(cons <+> hsep(types map typeDoc))
      case FunTy(types, _)        => catList(types map typeDoc, arrowLex)
    }

    implicit def exprDoc(t: Expr): Doc = t match {
      case ExVar(ide, _)           => ide
      case ExCon(con, _)           => con
      case ConstInt(n, _)          => value(n)
      case ConstChar(char, _)      => dquotes(value(char))
      case ConstString(str, _)     => dquotes(value(str))
      case Lambda(pats, e, _)      => parens(lambdaLex <+> list(pats, "", patternDoc _, "") <+> dotLex <> nest(line <> e))
      case App(f, e, _)            => parens(f <+> e)
      case Case(e, alts, _)        => caseLex <+> e <@> ssep(alts map altDoc, linebreak)
      case Conditional(c, t, e, _) => ifLex <+> c <@> thenLex <+> nest(line <> t) <@> elseLex <> nest(line <> e)
      case Let(ldefs, e, _)        => letLex <+> nest(line <> cat(ldefs map letDefDoc)) <@> inLex <> nest(line <> e)
    }

    implicit def letDefDoc(ldef: LetDef): Doc = ldef.lhs <+> funEqLex <+> ldef.rhs

    implicit def altDoc(alt: Alternative): Doc = ofLex <+> alt.pattern <+> thenLex <+> nest(alt.expr)

    implicit def patternDoc(p: Pattern): Doc = p match {
      case PatternVar(v, _)           => v
      case PatternExpr(cons, pats, _) => cons <+> catList(pats map patternDoc, "")
    }

    def catList(list: List[Doc], sep: Doc): Doc = (group(nest(lsep(list, sep))))
  }

  
}


/**
  * The type aliases are needed at some points for instantiating traits,
  * thus we need a companion object to be able to import these type values.
  */
object Syntax {
  type Var = String

  type TypeVar = String

  type ConVar = String

  type TConVar = String
}
