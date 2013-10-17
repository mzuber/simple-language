/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

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

/**
 * The type aliases are needed at some points for instantiating traits,
 * thus we need a companion object to be able to import these type values.
 */
object Syntax {
  /*
   * All identifiers are currently represented by strings (since we do not
   * use a symbol table).
   */

  // TODO: Rename those types such that it's clear that they represent the types for certain kinds of identifiers

  type VarName = String

  type TypeVarName = String

  type ConVarName = String

  type TConVarName = String

  type ModuleVar = String
  val LocalMod: ModuleVar = ""

  abstract class QualifiedVar(val module: ModuleVar = LocalMod) {
    def isLocal() = (module == LocalMod)
    def nameToString(): String;

    override def equals(that: Any) = {
      that != null &&
      that.isInstanceOf[QualifiedVar] &&
      this.module == that.asInstanceOf[QualifiedVar].module &&
      this.nameToString() == that.asInstanceOf[QualifiedVar].nameToString()
    }
    
    override def hashCode() = {
      this.module.hashCode() + this.nameToString().hashCode()
    }
    
    override def toString() = {
      if (module == LocalMod) {
        nameToString
      } else {
        module + "." + nameToString
      }
    }
  }
  
  abstract class VarFirstClass(val ide: VarName, override val module: ModuleVar = LocalMod) extends QualifiedVar(module) {
    override def nameToString = ide
  }

  case class Var(override val ide: VarName, override val module: ModuleVar = LocalMod) extends VarFirstClass(ide, module)
  // Even though there is TypeVarName, TypeVar does not exist.
  case class ConVar(override val ide: ConVarName, override val module: ModuleVar = LocalMod) extends VarFirstClass(ide, module)  
  case class TConVar(ide: TConVarName, override val module: ModuleVar = LocalMod) extends QualifiedVar(module) {
    override def nameToString = ide
  }
}

/**
 * Abstract syntax of SL
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
  case class FileLocation(file: String, from: Position, to: Position) extends Location {
    override def toString(): String = 
      if (from == null || to == null) {
        // for imported names, no line is given.
        file
      } else if (from.line == to.line) {
        if (from.col == to.col)
          file + ":" + from.line.toString + ":" + from.col.toString
        else
          file + ":" + from.line.toString + ":" + from.col.toString + "-" + to.col.toString
      } else {
        file + ":" + from.line.toString + "-" + to.line.toString
      }
  }
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
    override def hashCode(): Int = { classOf[Attribute].hashCode() }
    override def equals(other: Any) = other match {
      case a: Attribute => true
      case _ => false
    }
  }
  case class AttributeImpl(location: Location) extends Attribute {
    override def toString() = location.toString
    override def hashCode(): Int = { classOf[Attribute].hashCode() }
    override def equals(other: Any) = other match {
      case a: Attribute => true
      case _ => false
    }
  }
  case object EmptyAttribute extends Attribute {
    override def toString() = "Unknown location"
    override def hashCode(): Int = { classOf[Attribute].hashCode() }
    override def equals(other: Any) = other match {
      case a: Attribute => true
      case _ => false
    }
  }

  type VarName = Syntax.VarName
  type TypeVarName = Syntax.TypeVarName
  type ConVarName = Syntax.ConVarName
  type TConVarName = Syntax.TConVarName
  type ModuleVar = Syntax.ModuleVar
  
  type VarFirstClass = Syntax.VarFirstClass
  type Var = Syntax.Var
  //type TypeVar = Syntax.TypeVar // Should not exist...
  type ConVar = Syntax.ConVar
  type TConVar = Syntax.TConVar

  val LocalMod: ModuleVar = Syntax.LocalMod

  /**
   *
   */
  sealed abstract class AST {
    override def toString(): String = ASTPrettyPrinter.pretty(this)
  }

  /**
   * A program consists of top level definitions which might be
   * annotated with an explicit type signature.
   */
  case class Program(
      imports: List[Import],
      signatures: Map[VarName, FunctionSig],
      functionDefs: Map[VarName, List[FunctionDef]],
      functionDefsExtern: Map[VarName, FunctionDefExtern],
      dataDefs: List[DataDef],
      attribute: Attribute = EmptyAttribute)
    extends AST
    
  val emptyProgram = Program(List(), Map(), Map(), Map(), List())

  abstract class Import(val path: String, val attribute: Attribute = EmptyAttribute)

  case class UnqualifiedImport(
      override val path: String,
      override val attribute: Attribute = EmptyAttribute)
    extends Import(path, attribute)
  
  case class QualifiedImport(
      override val path: String,
      val name: ModuleVar,
      override val attribute: Attribute = EmptyAttribute)
    extends Import(path, attribute)
  
  /** Special import that allows to directly include js. */ 
  case class ExternImport(
      override val path: String,
      override val attribute: Attribute = EmptyAttribute)
    extends Import(path, attribute)

  sealed abstract class DeclarationModifier
  case object DefaultModifier extends DeclarationModifier
  case object PublicModifier extends DeclarationModifier
  
  /**
   * Type signature for top-level function definitions.
   */
  case class FunctionSig(
      typ: ASTType,
      modifier: DeclarationModifier = DefaultModifier,
      attribute: Attribute = EmptyAttribute)
  
  /**
   *  Top-level function definitions.
   */
  case class FunctionDef(
      patterns: List[Pattern],
      expr: Expr,
      attribute: Attribute = EmptyAttribute)
  
  /**
   *  Top-level function definitions by mapping to some extern js function.
   */
  case class FunctionDefExtern(val externName: String, attribute: Attribute = EmptyAttribute) // BUG bad naming: externName is a JS-block
  
  /**
   * Patterns for top-level function definitions.
   */
  sealed abstract class Pattern
  case class PatternVar(ide: VarName, attribute: Attribute = EmptyAttribute) extends Pattern
  case class PatternExpr(con: ConVar, patExprs: List[Pattern], attribute: Attribute = EmptyAttribute) extends Pattern

  def varsList(p: Pattern): List[VarName] = p match {
    case PatternVar(x, _) => List(x)
    case PatternExpr(_, sub, _) => sub.flatMap(vars)
  }

  def vars(p: Pattern): Set[VarName] = varsList(p).toSet

  /**
   * Data type definitions.
   */
  case class DataDef(
      ide: TConVarName,
      tvars: List[TypeVarName],
      constructors: List[ConstructorDef],
      modifier: DeclarationModifier = DefaultModifier,
      attribute: Attribute = EmptyAttribute)

  /**
   * A datatype consists of a list of data constructors CondDef and
   * the types of the arguments they take.
   */
  case class ConstructorDef(constructor: ConVarName, types: List[ASTType], attribute: Attribute = EmptyAttribute)

  /**
   * All type constructors of a program.
   */ 
  def allTypeCons(dataDefs: List[DataDef], module : ModuleVar = LocalMod): List[TConVar] =
    dataDefs.map(_.ide).map(Syntax.TConVar(_, module))

  /**
   * All data constrcutor of a program.
   */
  def allDataCons(dataDefs: List[DataDef]): List[ConVar] =
    dataDefs.flatMap(_.constructors).map(_.constructor).map(Syntax.ConVar(_))

  /**
   * All applications of type constructors in type definitions.
   */
  def allAppTypeCons(conDefs: List[ConstructorDef]): List[TConVar] = {
    val conTypes: List[ASTType] = conDefs flatMap (_.types)

    def selectAppTypeCon(ty: ASTType) = ty match {
      case TyExpr(conType, _, _) => List(conType)
      case _ => Nil
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
      case _ => 0
    }
  }

  case class TyVar(ide: TypeVarName, attribute: Attribute = EmptyAttribute) extends ASTType
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
  case class ConstReal(value: Double, attribute: Attribute = EmptyAttribute) extends Expr
  case class JavaScript(jsCode: String, signature: Option[ASTType], attribute: Attribute = EmptyAttribute) extends Expr

  /**
   * Local definition in a let-binding.
   */
  case class LetDef(lhs: VarName, rhs: Expr, attribute: Attribute = EmptyAttribute)

  /**
   * Alternative in a case expression.
   */
  case class Alternative(pattern: Pattern, expr: Expr, attribute: Attribute = EmptyAttribute)

  /**
   * Select the attribute of an expression.
   */
  def attribute(expr: Expr): Attribute = expr match {
    case Conditional(_, _, _, attr) => attr
    case Lambda(_, _, attr) => attr
    case Case(_, _, attr) => attr
    case Let(_, _, attr) => attr
    case App(_, _, attr) => attr
    case ExVar(_, attr) => attr
    case ExCon(_, attr) => attr
    case ConstInt(_, attr) => attr
    case ConstReal(_, attr) => attr
    case ConstChar(_, attr) => attr
    case ConstString(_, attr) => attr
    case JavaScript(_, _, attr) => attr
  }

  //TODO: test!
  def fv(fd: FunctionDef): Set[Var] = {
    fv(fd.expr) -- (for (p <- fd.patterns; v <- vars(p)) yield Syntax.Var(v))
  }

  def fv(expr: Expr): Set[Var] = expr match {
    case ExCon(_, _) => Set()
    case ConstInt(_, _) => Set()
    case ConstChar(_, _) => Set()
    case ConstString(_, _) => Set()
    case ConstReal(_, _) => Set()
    case JavaScript(_, _, _) => Set()
    case ExVar(x, _) => Set(x)
    case App(l, r, _) => fv(l) ++ fv(r)
    case Let(defs, rhs, _) => fv(rhs) ++ (for (d <- defs; v <- fv(d.rhs)) yield v) -- defs.map(_.lhs).map(Syntax.Var(_))
    case Conditional(c, t, e, _) => fv(c) ++ fv(t) ++ fv(e)
    case Lambda(p, rhs, _) => fv(rhs) -- (for (pat <- p; v <- vars(pat)) yield Syntax.Var(v))
    case Case(e, a, _) => fv(e) ++ (for (alt <- a; v <- fv(alt.expr)) yield v) -- 
    (for (alt <- a; v <- vars(alt.pattern)) yield Syntax.Var(v))
  }

  /**
   * Pretty printer for SL definitions and expressions.
   */
  object ASTPrettyPrinter extends org.kiama.output.PrettyPrinter with Lexic {

    def pretty(t: Any): String = t match {
      case e: Expr => super.pretty(showExpr(e))
      case m: Program => super.pretty(showProgram(m))
      case i: Import => super.pretty(showImport(i))
      case e => pretty_any(e)
    }

    def showProgram(m: Program): Doc = {
      var doc = line

      for (i <- m.imports)
        doc = doc <@> showImport(i)

      for (d <- m.dataDefs)
        doc = doc <@> showDataDef(d)

      for ((name, s) <- m.signatures)
        doc = doc <@> showModifier(s.modifier) <+> funLex <+> name <+> typeLex <+> showType(s.typ) <> line

      for (
        (name, d) <- m.functionDefsExtern
      ) {
        doc = doc <@> defLex <+> externLex <+> name <+> 
        	    funEqLex <+> dquotes(value(d.externName)) <> line
      }
        
      for (
        (name, ds) <- m.functionDefs;
        d <- ds
      ) {
        doc = doc <@> defLex <+> name <+> 
        	catList(d.patterns.map(showPattern), "") <+> funEqLex <+> showExpr(d.expr) <> line
      }
      doc
    }

    def showImport(i: Import) : Doc = i match {
      case QualifiedImport(path, name, _) => importLex <+> dquotes(path) <+> asLex <+> name
      case ExternImport(path, _) => importLex <+> externLex <+> dquotes(path)
    }

    def showDataDef(d: DataDef): Doc = d match {
      case DataDef(name, Nil, cons, modifier, _) => {
        showModifier(modifier) <+> dataLex <+> d.ide <+> funEqLex <+> catList(d.constructors map showConstructor, space <> dataSepLex) <> line
      }
      case DataDef(name, vars, cons, modifier, _) => {
        showModifier(modifier) <+> dataLex <+> d.ide <+> hsep(d.tvars.map(value)) <+> funEqLex <+> catList(d.constructors map showConstructor, space <> dataSepLex) <> line
      }
    }
    
    def showModifier(m: DeclarationModifier): Doc = m match {
      case DefaultModifier => empty
      case PublicModifier => publicLex
    }

    def showConstructor(c: ConstructorDef) = c.constructor <+> hsep(c.types.map(showType))

    def showType(t: ASTType): Doc = t match {
      case TyVar(i, a) => i
      case TyExpr(c, Nil, _) => c.toString
      case TyExpr(c, ts, a) => parens(c.toString <+> hsep(ts.map(showType)))
      case FunTy(ps, a) => parens(catList(ps map showType, arrowLex))
    }

    def showExpr(t: Expr): Doc = t match {
      case Conditional(c, t, e, a) => ifLex <+> showExpr(c) <@> thenLex <+> nest(line <> showExpr(t)) <@> elseLex <> nest(line <> showExpr(e))
      case Lambda(ps, e, a) => parens(lambdaLex <+> catList(ps.map(showPattern), "") <+> dotLex <> nest(line <> showExpr(e)))
      case Case(e, as, a) => caseLex <+> showExpr(e) <@> ssep(as.map(showAlt), linebreak)
      case Let(ds, e, a) => letLex <+> nest(line <> cat(ds.map(showLefDef))) <@> inLex <> nest(line <> showExpr(e))
      case App(f, e, a) => parens(showExpr(f) <+> showExpr(e))
      case ExVar(i, a) => i.toString
      case ExCon(c, a) => c.toString
      case ConstInt(v, a) => value(v)
      case ConstChar(c, a) => dquotes(value(c))
      case ConstString(s, a) => dquotes(value(s))
      case ConstReal(x, _) => x.toString
      case JavaScript(j, s, a) => {
        val sigDoc = s match {
          case None => empty
          case Some(sig) => " :" <+> showType(sig)
        }
        jsOpenLex <+> j <+> jsCloseLex <> sigDoc
      }
    }

    def showLefDef(l: LetDef): Doc = text(l.lhs) <+> funEqLex <+> showExpr(l.rhs)
    def showAlt(a: Alternative): Doc = ofLex <+> showPattern(a.pattern) <+> thenLex <+> nest(showExpr(a.expr))
    def showPattern(p: Pattern): Doc = p match {
      case PatternVar(v, a) => v
      case PatternExpr(c, ps, a) => 
        if (ps.isEmpty) {
          text(c.toString)
        } else {
          parens(text(c.toString) <+> catList(ps.map(showPattern), ""))
        }
    }

    def catList(l: List[Doc], sep: Doc): Doc = (group(nest(lsep(l, sep))))
  }

}
