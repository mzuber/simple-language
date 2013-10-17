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

package de.tuberlin.uebb.sl2.impl

import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

import de.tuberlin.uebb.sl2.modules._

import org.parboiled.scala.{Parser => PBParser, _}
import org.parboiled.Context
import org.parboiled.errors.{ParseError => PBParseError}
import org.parboiled.errors.ErrorUtils._
import org.parboiled.buffers.DefaultInputBuffer

/**
  * Parser implementation module based on the PEG parser library parboiled
  * (see [[http://www.parboiled.org]]
  */
trait ParboiledParser extends PBParser with Parser with Lexic with Syntax with Errors {

  //(Next 3 defs copied from CombinatorParser.scala
  //contains the offsets where a line starts
  //in the input, which is currently parsed
  private var lines: Array[Int] = Array()
  private def buildLineIndex(s: CharSequence): Array[Int] = {
    val lines = new ArrayBuffer[Int]
    lines += 0
    for (i <- 0 until s.length)
      if (s.charAt(i) == '\n') lines += (i + 1)
    lines += s.length
    lines.toArray
  }

  private def offsetToPosition(off: Int): Position =
    {
      if (lines.length < 1) return Position(-1, -1)
      var min = 0
      var max = lines.length - 1
      while (min + 1 < max) {
        val mid = (max + min) / 2
        if (off < lines(mid))
          max = mid
        else
          min = mid
      }
      Position(min + 1, off - lines(min) + 1)
    }

  private def pbErr2Error(pe : PBParseError) : ParseError = {
    ParseError(pe.getErrorMessage, AttributeImpl(FileLocation(fileName, offsetToPosition(pe.getStartIndex), offsetToPosition(pe.getEndIndex))))
  }

  private def doParse[T](s : String, r : Rule1[T]) : Either[Error, T] = {
    val runner = ReportingParseRunner(r)
    val out : ParsingResult[T] = runner.run(s)
    out.result.map(r => Right(r)).getOrElse (Left(ErrorList(out.parseErrors map pbErr2Error)))    
  }

  def parseAst(in : String) : Either[Error, AST] = {
    lines = buildLineIndex(in)
    doParse(in, program)
  }

  def parseExpr(in : String) : Either[Error, Expr] = {
    doParse(in, expr)
  }
  
  var fileName = ""

  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with
   * a blank, this keeps the rules free from most whitespace matching clutter
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ spacing
    else
      str(string)

  /* boilerplate stuff, since there is no lexer in parboiled, we have to do this all on our own ... */
  def whiteSpace: Rule0 = rule { anyOf(" \n\r\t\f") }
  
  def comment : Rule0 = rule { "{-" ~ zeroOrMore(!("-}") ~ ANY) ~ "-}" }
    
  def oneLineComment : Rule0 = rule { "--" ~ zeroOrMore(!("\n") ~ ANY) ~ ("\n" | EOI) }

  def spacing : Rule0 = rule {
    zeroOrMore(whiteSpace | comment | oneLineComment )
  }

  def non_digit : Rule0 = rule { low_char | up_char }

  def low_char : Rule0 = rule { "_" | ("a" - "z") }

  def up_char : Rule0 = rule { "A" - "Z" }
  
  def digit: Rule0 = rule { "0" - "9" }

  def integer: Rule0 = rule { optional("-") ~ oneOrMore(digit) }

  def exponent : Rule0 = rule { ("E"|"e") ~ optional("-") ~ oneOrMore(digit) }

  def real : Rule0 = rule { 
    optional("-") ~ (
      oneOrMore(digit) ~ "." ~ zeroOrMore(digit) |
      "." ~ oneOrMore(digit)
    ) ~ optional(exponent)
  }
  
  def char: Rule0 = rule { "'" ~ !("'") ~ ANY ~ "'" }

  /**
   * avoid parsing keywords as ide-prefixes
   */
  def keyword : Rule0 = rule { ("FUN" | "DEF" | "IF" | "THEN" | "ELSE" |
      "LET" | "IN" | "CASE" | "OF" | "DATA" | "IMPORT" | "AS" | "EXTERN" | "PUBLIC") ~
      !(digit | non_digit) }

  def kw(string : String) : Rule0 = {
    string ~ !(digit | non_digit) ~ spacing
  }
  
  def low_ident_token : Rule0 = rule {
    (!keyword) ~ low_char ~ zeroOrMore(digit | non_digit)
  }

  def up_ident_token : Rule0 = rule {
    (!keyword) ~ up_char ~ zeroOrMore(digit | non_digit)
  }

  def string_token : Rule1[String] = rule {
    "\"" ~ (zeroOrMore(character) ~> {x => x}) ~ "\""
  }

  def js_token : Rule1[String] = rule {
    jsOpenLex ~ (zeroOrMore(!jsCloseLex ~ ANY) ~> (s => s)) ~ jsCloseLex
  }

  def character: Rule0 = rule { s_escape | s_char }

  def s_escape: Rule0 = rule { "\\" ~ (anyOf("\"\\/bfnrt")) }

  def s_char: Rule0 = rule { !anyOf("\"\\") ~ ANY }
  
  /* actual AST generating rules */

  def mkQualImport(path : String, mod : String) = QualifiedImport(path, mod)

  def mkExternImport(path : String) = ExternImport(path)
  
  def mkQualVar(mod : String, name : String) = ExVar(Syntax.Var(name, mod))

  def mkQualConVar(mod : String, name : String) = ExCon(Syntax.ConVar(name, mod))

  def mkQualType(mod : String, name : String) = TyExpr(Syntax.TConVar(name, mod), Nil)

  def mkQualTyExpr(mod : String, name : String, ts : List[ASTType]) = TyExpr(Syntax.TConVar(name, mod), ts)

  def mkApp(e1 : Expr, e2 : Expr) = App(e1,e2)

  def mkOp(e1 : Expr, e2 : Expr) = App(e2,e1)

  def mkIf(c:Expr, t : Expr, e : Expr) = Conditional(c, t, e)

  def mkLetDef(x : String, e : Expr) = LetDef(x, e)

  def mkLet(ld : List[LetDef], e : Expr) = Let(ld, e)

  def mkNeg(e : Expr) = e match {
    case ConstReal(r, attr) => ConstReal(-r, attr)
    case ConstInt(n, attr) => ConstInt(-n, attr)
    case e2@_ => App(App(ExVar(Syntax.Var(mulLex)), ConstInt(-1)), e2)
  }

  def mkCase(e : Expr, a : List[Alternative]) = {  Case(e, a)  }

  def mkAlt(p : Pattern, e : Expr) = { Alternative(p, e) }

  def mkJs(s : String, ty : Option[ASTType]) = { JavaScript(s, ty) }

  /* Program constructors */

  def mkExternFun(s : String, jsName: String) = (s -> FunctionDefExtern(jsName))
  
  def mkFun(s : String, pattern : List[Pattern], e : Expr) = (s -> FunctionDef(pattern, e))

  def mkCustomOp(l : Pattern, s : String, r : Pattern, e : Expr) = (s -> FunctionDef(l::r::Nil, e))

  def mkProgram() = emptyProgram

  private def updateMap[T](s : String, t : T, m : Map[String, List[T]]) =
    m + (s -> (t::m.get(s).getOrElse(Nil)))

  def mkProgramImport(a : AST, x : Import) = a match {
    case m:Program => m.copy(imports = x::m.imports)
  }

  def mkProgramExternDef(a : AST, x : (String, FunctionDefExtern)) = a match {
    case m:Program => m.copy(functionDefsExtern = m.functionDefsExtern + ((x._1, x._2)))
  }
  
  def mkProgramDef(a : AST, x : (String, FunctionDef)) = a match {
    case m:Program => m.copy(functionDefs = updateMap(x._1, x._2, m.functionDefs))
  }

  def mkProgramSig(a : AST, s : String, f : FunctionSig) = a match {
    case m:Program => m.copy(signatures = m.signatures + (s -> f))
  }

  def mkProgramData(a : AST, d : DataDef) = a match {
    case m:Program => m.copy(dataDefs = d::m.dataDefs)
  }

  def mkFunSig(m : DeclarationModifier)(t : ASTType) = FunctionSig(t, m)
  
  def mkTyExpr(s : String, ts : List[ASTType]) = TyExpr(Syntax.TConVar(s), ts)

  def mkFunType(t : ASTType, ts : List[ASTType]) = FunTy(t::ts)
  
  def mkDataDef(m : DeclarationModifier)(s : String, vs : List[String], cs : List[ConstructorDef]) = DataDef(s, vs, cs, m)
  
  def mkConsDef(s : String, ts : List[ASTType]) = ConstructorDef(s, ts)

  /* some lexical helper rules */
  def variable : Rule1[String] = rule {
    low_ident_token ~> (x => x) ~ spacing
  }

  def constructor : Rule1[String] = rule {
    up_ident_token ~> (s => s) ~ spacing
  }

  def qualification : Rule1[String] = rule {
    up_ident_token ~> (s => s) ~ spacing ~ "." ~ spacing
  }

  def custom_op : Rule1[Expr] = rule {
    custom_op_token ~~> (s => ExVar(Syntax.Var(s))) |
    qualification ~ custom_op_token ~~> (mkQualVar _)
  }
  
  def custom_op_token : Rule1[String] = rule {    
    // TODO: if we change binding we might need to check for builtin ops first
    //(!builtin_op) ~ oneOrMore(anyOf("!§%&/=?+*#-:<>|")) ~> (s => ExVar(s)) ~ spacing
    oneOrMore(anyOf("!§%&/=?+*#-<>|")) ~> (s => s) ~ spacing
  }

  def relation_op : Rule1[Expr] = rule {
    relation_op_token ~~> (s => ExVar(Syntax.Var(s))) |
    qualification ~ relation_op_token ~~> (mkQualVar _)
  }

  def relation_op_token : Rule1[String] = rule {
    ("<"|">"|"<="|">="|"/="|"==") ~> (s => s) ~ spacing
  }

  def arith_op : Rule1[Expr] = rule {
    arith_op_token ~~> (s => ExVar(Syntax.Var(s))) |
    qualification ~ arith_op_token ~~> (mkQualVar _)
  }

  def arith_op_token : Rule1[String] = rule {
    anyOf("+-") ~> (s => s) ~ spacing
  }

  def term_op : Rule1[Expr] = rule {
    term_op_token ~~> (s => ExVar(Syntax.Var(s))) |
    qualification ~ term_op_token ~~> (mkQualVar _)
  }

  def term_op_token : Rule1[String] = rule {
    anyOf("*/") ~> (s => s) ~ spacing
  }

  /**
   * parse the program parts
   */
  def program : Rule1[AST] = rule {
    push(mkProgram()) ~ spacing ~ zeroOrMore(continueProgram) ~ EOI
  }

  def continueProgram : ReductionRule1[AST, AST] = rule {
    import_def ~~> (mkProgramImport _) |
    fun_extern_def ~~> (mkProgramExternDef _) | 
    fun_def ~~> (mkProgramDef _) |
    data_def ~~> (mkProgramData _) |
    fun_sig ~~> (mkProgramSig _) 
  }

  def import_def : Rule1[Import] = rule {
    (kw("IMPORT") ~ spacing ~ kw("EXTERN") ~ spacing ~ string_token ~ spacing ~~> (mkExternImport _)) |
    (kw("IMPORT") ~ string_token ~ spacing  ~ kw("AS") ~ constructor ~~> (mkQualImport _))
  }

  def fun_extern_def : Rule1[(String, FunctionDefExtern)] = rule {
    kw("DEF") ~ kw("EXTERN") ~ (variable|custom_op_token) ~
    		"= " ~ js_token ~ spacing ~~> (mkExternFun)
  }
  
  def fun_def : Rule1[(String, FunctionDef)] = rule {
    kw("DEF") ~ (
      variable ~ zeroOrMore(def_pattern) ~ "= " ~ expr ~~> (mkFun _) |
      def_pattern ~ custom_op_token ~ def_pattern ~ "= " ~ expr ~~> (mkCustomOp _) 
    )
  }
  
  def data_def : Rule1[DataDef] = rule {
    (kw("PUBLIC") ~ kw("DATA") ~ constructor ~
    	(zeroOrMore(variable) ~ "= " ~ oneOrMore(cons_def, "| ") ~~> mkDataDef(PublicModifier) _)) |
    (kw("DATA") ~ constructor ~
    	(zeroOrMore(variable) ~ "= " ~ oneOrMore(cons_def, "| ") ~~> mkDataDef(DefaultModifier) _))
  }

  def cons_def : Rule1[ConstructorDef] = rule {
    constructor ~ zeroOrMore(cons_def_element) ~~> (mkConsDef _)
  }

  def cons_def_element : Rule1[ASTType] = rule {
    constructor ~ !(".") ~~> (c => mkTyExpr(c, Nil)) |
    qualification ~ constructor ~~> (mkQualType _) |
    "( " ~ type_expr ~ ") " |
    variable ~~> (s => TyVar(s))
  }

  def fun_sig : Rule2[String, FunctionSig] = rule {
    kw("PUBLIC") ~ kw("FUN") ~ (variable | custom_op_token) ~ ": " ~ type_expr ~~> (mkFunSig(PublicModifier) _) |
    kw("FUN") ~ (variable | custom_op_token) ~ ": " ~ type_expr ~~> (mkFunSig(DefaultModifier) _)
  }
  
  def type_expr : Rule1[ASTType] = rule { type_expr_base ~ optional(fun_rhs) }

  def type_expr_base : Rule1[ASTType] = rule {
    constructor ~ !(".") ~ zeroOrMore(type_arg) ~~> (mkTyExpr _) |
    qualification ~ constructor ~ zeroOrMore(type_arg) ~~> (mkQualTyExpr _) |
    "( " ~ type_expr ~ ") " |
    variable ~~> (s => TyVar(s))
  }

  def type_arg : Rule1[ASTType] = rule {
    constructor ~ !(".") ~~> (s => TyExpr(Syntax.TConVar(s), List())) |
    qualification ~ constructor ~~> ((m, s) => TyExpr(Syntax.TConVar(s, m), List())) |
    "( " ~ type_expr ~ ") " |
    variable ~~> (s => TyVar(s))
  }

  def fun_rhs : ReductionRule1[ASTType, ASTType] = rule {
    oneOrMore("-> " ~ type_expr_base) ~~> (mkFunType _)
  }

  /**
   * parse an expression
   */
  def expr : Rule1[Expr] = rule { 
    kw("CASE") ~ expr ~ oneOrMore(alternative) ~~> (mkCase _) |
    kw("IF") ~ expr ~ kw("THEN") ~ expr ~ kw("ELSE") ~ expr ~~> (mkIf _) |
    (kw("LET") ~ oneOrMore(let_def) ~ kw("IN") ~ expr ~~> (mkLet _)) |
    customOpApp
  }

  def let_def : Rule1[LetDef] = rule {
    (variable ~ "= " ~ expr) ~~> (mkLetDef _)
  }

  def alternative : Rule1[Alternative] = rule {
    kw("OF") ~ pattern ~ kw("THEN") ~ expr ~~> (mkAlt _)
  }

  def customOpApp : Rule1[Expr] = rule {
    relation ~ zeroOrMore(custom_op_rhs)
  }

  def custom_op_rhs : ReductionRule1[Expr, Expr] = {
    (custom_op ~~> (mkOp _)) ~ (relation ~~> (mkApp _))
  }

  def relation : Rule1[Expr] = rule {      
    arith ~ zeroOrMore(relation_rhs)
  }
  
  def relation_rhs : ReductionRule1[Expr,Expr] = rule {
    (relation_op ~~> (mkOp _)) ~ (arith ~~> (mkApp _))
  }

  def arith : Rule1[Expr] = rule {
    arith_lhs ~ zeroOrMore(arith_rhs)
  }

  def arith_lhs : Rule1[Expr] = rule {
    // TODO find some less dangerous way to implement negation
    // "- " ~ term ~~> (mkNeg _) |
    term
  }

  def arith_rhs : ReductionRule1[Expr, Expr] = rule {
    (arith_op ~~> (mkOp _)) ~ (term ~~> (mkApp _))
  }

  def term : Rule1[Expr] = {
    factor ~ zeroOrMore(term_rhs)
  }

  def term_rhs : ReductionRule1[Expr,Expr] = rule {
    (term_op ~~> (mkOp _)) ~ (factor ~~> (mkApp _))
  }   

  def factor : Rule1[Expr] = rule {
    primary ~ zeroOrMore(factor_rhs)
  }

  def factor_rhs : ReductionRule1[Expr, Expr] = rule {
    primary ~~> (mkApp _)
  }

  def mkLam(p : List[Pattern], e : Expr) = { Lambda(p, e) }

  def primary : Rule1[Expr] = rule {
    "\\ " ~ oneOrMore(def_pattern) ~ ". " ~ expr ~~> (mkLam _) |
    "( " ~ expr ~ ") " |
    real ~> (s => ConstReal(s.toDouble)) ~ spacing |
    integer ~> (s => ConstInt(s.toInt)) ~ spacing | 
    char ~> (s => ConstChar(s(1))) ~ spacing |
    (low_ident_token ~> (s => ExVar(Syntax.Var(s)))) ~ spacing ~ !("=" ~ !("=")) |
    up_ident_token ~> (s => ExCon(Syntax.ConVar(s))) ~ spacing ~ !(".") |          // constructor, not module
    qualification ~ variable ~~> (mkQualVar _) |
    qualification ~ constructor ~~> (mkQualConVar _) |
    js_token ~ spacing ~ optional(": " ~ type_expr) ~~> (mkJs _) ~ spacing | 
    string_token ~~> (s => ConstString(s)) ~ spacing    
  }

  def mkPattern(c : String, p : List[Pattern]) = { 
    PatternExpr(Syntax.ConVar(c), p)
  }

  def mkQualPattern(mod : String, c : String, p : List[Pattern]) = {
    PatternExpr(Syntax.ConVar(c, mod), p)
  }

  /* Pattern syntax in DEFs and CASEs differ */

  def def_pattern : Rule1[Pattern] = rule {
    "( " ~ qualification ~ constructor ~ pattern_rhs ~~> (mkQualPattern _) ~ ") " |
    "( " ~ constructor ~ pattern_rhs ~~> (mkPattern _) ~ ") " |
    low_ident_token ~> (s => PatternVar(s)) ~ spacing |
    qualification ~ constructor ~ push(Nil) ~~> (mkQualPattern _) |
    constructor ~ push(Nil) ~~> (mkPattern _)
  }

  def pattern : Rule1[Pattern] = rule {
    "( " ~ pattern ~ ") " |
    low_ident_token ~> (s => PatternVar(s)) ~ spacing |
    qualification ~ constructor ~ pattern_rhs ~~> (mkQualPattern _) |
    constructor ~ pattern_rhs ~~> (mkPattern _)
  }

  def pattern_rhs : Rule1[List[Pattern]] = rule {
    oneOrMore(def_pattern) |
    push(Nil)
  }
}
