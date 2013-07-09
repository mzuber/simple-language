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

import scala.language.implicitConversions

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

  private def pbErr2Error(pe : PBParseError) : ParseError = {
    ParseError(pe.getErrorMessage, pe.getStartIndex, pe.getEndIndex)
  }

  private def doParse[T](s : String, r : Rule1[T]) : Either[Error, T] = {
    val runner = ReportingParseRunner(r)
    val out : ParsingResult[T] = runner.run(s)
    out.result.map(r => Right(r)).getOrElse (Left(ErrorList(out.parseErrors map pbErr2Error)))    
  }

  def parseAst(in : String) : Either[Error, AST] = {
    doParse(in, program)
  }

  def parseExpr(in : String) : Either[Error, Expr] = {
    doParse(in, expr)
  }
  
  var fileName = ""

  /**
    * We redefine the default string-to-rule conversion to also match trailing whitespace if the
    * string ends with a blank, this keeps the rules free from most whitespace matching clutter.
    */
  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ spacing
    else
      str(string)

  /* Boilerplate stuff, since there is no lexer in parboiled, we have to do this all on our own ... */
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

  def integer: Rule0 = rule { oneOrMore(digit) }
  
  def char: Rule0 = rule { "'" ~ !("'") ~ ANY ~ "'" }
  
  /* Avoid parsing keywords as ide-prefixes */
  def keyword : Rule0 = rule { ("DEF" | "FUN" | "IF" | "THEN" | "ELSE" | "LET" | "IN" | "CASE" | "OF" | "DATA") ~ !(digit | non_digit) }

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

  def character: Rule0 = rule { s_escape | s_char }

  def s_escape: Rule0 = rule { "\\" ~ (anyOf("\"\\/bfnrt")) }

  def s_char: Rule0 = rule { !anyOf("\"\\") ~ ANY }
  
  /* Actual AST generating rules */

  def mkApp(e1 : Expr, e2 : Expr) = App(e1,e2)

  def mkOp(e1 : Expr, e2 : Expr) = App(e2,e1)

  def mkIf(c:Expr, t : Expr, e : Expr) = Conditional(c, t, e)

  def mkLetDef(x : String, e : Expr) = LetDef(x, e)

  def mkLet(ld : List[LetDef], e : Expr) = Let(ld, e)

  def mkNeg(e : Expr) = e match {
    case ConstInt(n, attr) => ConstInt(-n, attr)
    case e2@_ => App(App(ExVar(mulLex), ConstInt(-1)), e2)
  }

  def mkCase(e : Expr, a : List[Alternative]) = {  Case(e, a)  }

  def mkAlt(p : Pattern, e : Expr) = { Alternative(p, e) }

  /* Program constructors */

  def mkFun(s : String, pattern : List[Pattern], e : Expr) = (s -> FunctionDef(pattern, e))

  def mkCustomOp(l : Pattern, s : String, r : Pattern, e : Expr) = (s -> FunctionDef(l::r::Nil, e))

  def mkProgram() = {
    Program(Map(), Map(), Nil)
  }

  private def updateMap[T](s : String, t : T, m : Map[String, List[T]]) = m + (s -> (t::m.get(s).getOrElse(Nil)))

  def mkProgramDef(a : AST, x : (String, FunctionDef)) = a match {
    case m:Program => m.copy(functionDefs = updateMap(x._1, x._2, m.functionDefs))
  }

  def mkProgramSig(a : AST, s : String, f : FunctionSig) = a match {
    case m:Program => m.copy(signatures = m.signatures + (s -> f))
  }

  def mkProgramData(a : AST, d : DataDef) = a match {
    case m:Program => m.copy(dataDefs = d::m.dataDefs)
  }

  def mkFunSig(t : ASTType) = FunctionSig(t)
  
  def mkTyExpr(s : String, ts : List[ASTType]) = TyExpr(s, ts)

  def mkFunType(t : ASTType, ts : List[ASTType]) = FunTy(t::ts)
  
  def mkDataDef(s : String, vs : List[String], cs : List[ConstructorDef]) = DataDef(s, vs, cs)
  
  def mkConsDef(s : String, ts : List[ASTType]) = ConstructorDef(s, ts)

  /* Some lexical helper rules */
  def variable : Rule1[String] = rule {
    low_ident_token ~> (x => x) ~ spacing
  }

  def constructor : Rule1[String] = rule {
    up_ident_token ~> (s => s) ~ spacing
  }

  def builtin_op : Rule0 = rule {
    (NOTHING /: predefinedOps)({(x,y) => x | y}) ~ (!anyOf("!§%&/=?+*#-:<>|"))
  }

  def custom_op : Rule1[Expr] = rule {
    custom_op_token ~~> (s => ExVar(s))
  }

  def custom_op_token : Rule1[String] = rule {    
    // TODO: if we change binding we might need to check for builtin ops first
    //(!builtin_op) ~ oneOrMore(anyOf("!§%&/=?+*#-:<>|")) ~> (s => ExVar(s)) ~ spacing
    oneOrMore(anyOf("!§%&/=?+*#-:<>|")) ~> (s => s) ~ spacing
  }

  /**
    * Parse the program parts.
    */
  def program : Rule1[AST] = rule {
    push(mkProgram()) ~ spacing ~ zeroOrMore(continueProgram)
  }

  def continueProgram : ReductionRule1[AST, AST] = rule {
    fun_def ~~> (mkProgramDef _) |
    data_def ~~> (mkProgramData _) |
    fun_sig ~~> (mkProgramSig _) 
  }

  def fun_def : Rule1[(String, FunctionDef)] = rule {
    kw("DEF") ~ (
      variable ~ zeroOrMore(def_pattern) ~ "= " ~ expr ~~> (mkFun _) |
      def_pattern ~ custom_op_token ~ def_pattern ~ "= " ~ expr ~~> (mkCustomOp _)
    )
  }
  
  def data_def : Rule1[DataDef] = rule {
    kw("DATA") ~ constructor ~ (zeroOrMore(variable) ~ "= " ~ oneOrMore(cons_def, "| ") ~~> (mkDataDef _)) 
  }

  def cons_def : Rule1[ConstructorDef] = rule {
    constructor ~ zeroOrMore(cons_def_element) ~~> (mkConsDef _)
  }

  def cons_def_element : Rule1[ASTType] = rule {
    constructor ~~> (c => mkTyExpr(c, Nil)) |
    "( " ~ type_expr ~ ") " |
    variable ~~> (s => TyVar(s))
  }

  def fun_sig : Rule2[String, FunctionSig] = rule {
    kw("FUN") ~ variable ~ ": " ~ type_expr ~~> (mkFunSig _)
  }
  
  def type_expr : Rule1[ASTType] = rule { type_expr_base ~ optional(fun_rhs) }

  def type_expr_base : Rule1[ASTType] = rule {
    constructor ~ zeroOrMore(type_expr) ~~> (mkTyExpr _) |
    "( " ~ type_expr ~ ") " |
    variable ~~> (s => TyVar(s))
  }

  def fun_rhs : ReductionRule1[ASTType, ASTType] = rule {
    oneOrMore("-> " ~ type_expr_base) ~~> (mkFunType _)
  }

  /**
    * Parse an expression.
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

  def gt : Rule1[Expr] = rule { "> " ~ push(ExVar(gtLex)) } 
  def lt : Rule1[Expr] = rule { "< " ~ push(ExVar(ltLex)) }
  def ge : Rule1[Expr] = rule { ">= " ~ push(ExVar(geLex)) } 
  def le : Rule1[Expr] = rule { "<= " ~ push(ExVar(leLex)) }
  def ne : Rule1[Expr] = rule { "/= " ~ push(ExVar(neLex)) }
  def eq : Rule1[Expr] = rule { "== " ~ push(ExVar(eqLex)) }
  
  def relation_rhs : ReductionRule1[Expr,Expr] = rule {
    (( gt | lt | ge | le | ne | eq ) ~~> (mkOp _)) ~ (arith ~~> (mkApp _))
  }

  def arith : Rule1[Expr] = rule {
    arith_lhs ~ zeroOrMore(arith_rhs)
  }

  def arith_lhs : Rule1[Expr] = rule {
    "- " ~ term ~~> (mkNeg _) |
    term
  }

  def add : Rule1[Expr] = rule { 
    "+s " ~ push(ExVar(strAdd)) |    
    "+ " ~ push(ExVar(addLex))
  }

  def sub : Rule1[Expr] = rule { 
    "- " ~ push(ExVar(subLex)) 
  }

  def arith_rhs : ReductionRule1[Expr, Expr] = rule {
    ((add | sub) ~~> (mkOp _)) ~ (term ~~> (mkApp _))
  }

  def term : Rule1[Expr] = {
    factor ~ zeroOrMore(term_rhs)
  }

  def mul : Rule1[Expr] = rule { 
    "* " ~ push(ExVar(mulLex)) 
  }

  def div : Rule1[Expr] = rule { 
    "/ " ~ push(ExVar(divLex)) 
  }

  def term_rhs : ReductionRule1[Expr,Expr] = rule {
    ((mul | div) ~~> (mkOp _)) ~ (factor ~~> (mkApp _))
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
    integer ~> (s => ConstInt(s.toInt)) ~ spacing | 
    char ~> (s => ConstChar(s(1))) ~ spacing |
    (low_ident_token ~> (s => ExVar(s))) ~ spacing ~ !("=" ~ !("=")) |
    up_ident_token ~> (s => ExCon(s)) ~ spacing |
    string_token ~~> (s => ConstString(s)) ~ spacing    
  }

  def mkPattern(c : String, p : List[Pattern]) = { 
    PatternExpr(c, p)
  }

  /* Pattern syntax in DEFs and CASEs differ */

  def def_pattern : Rule1[Pattern] = rule {
    "( " ~ (up_ident_token ~> (x => x)) ~ spacing ~ pattern_rhs ~~> (mkPattern _) ~ ") " |
    low_ident_token ~> (s => PatternVar(s)) ~ spacing |
    up_ident_token ~> (x => x) ~ spacing ~ push(Nil) ~~> (mkPattern _)
  }

  def pattern : Rule1[Pattern] = rule {
    "( " ~ pattern ~ ") " |
    low_ident_token ~> (s => PatternVar(s)) ~ spacing |
    up_ident_token ~> (x => x) ~ spacing ~ pattern_rhs ~~> (mkPattern _)
  }

  def pattern_rhs : Rule1[List[Pattern]] = rule {
    oneOrMore(def_pattern) |
    push(Nil)
  }
}
