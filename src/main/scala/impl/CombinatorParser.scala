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
import scala.language.postfixOps

import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.Parsers
import scala.util.matching.Regex

import org.kiama.output.PrettyPrinter

import de.tuberlin.uebb.sl2.modules._

/**
  * Parser implementation module based on Scala's parser combinators.
  */
trait CombinatorParser extends RegexParsers with Parsers with Parser with Syntax with Errors with Lexic {

  /**
    * Parse an SL program.
    */
  def parseAst(in: String): Either[Error, AST] = {
    lines = buildLineIndex(in)

    parseAll(parseTopLevel, new ParserString(in)) match {
      case Success(result, _) => Right(result)
      case NoSuccess(err, next) => Left(ParseError(err, next.pos.line, next.pos.column))
    }
  }

  /**
    * Parse an SL expression.
    */
  def parseExpr(in: String): Either[Error, Expr] = {
    lines = buildLineIndex(in)

    parseAll(expr, new ParserString(in)) match {
      case Success(result, _)   => Right(result)
      case NoSuccess(err, next) => Left(ParseError(err, next.pos.line, next.pos.column))
    }
  }

  /**
    * Global variable for the file currently proccessed by the parser.
    */
  var fileName = ""

  /* Contains the offsets where a line starts in the input, which is currently parsed. */
  private var lines: Array[Int] = Array()

  /**
    * Calculate a position based on the given offset.
    */
  private def offsetToPosition(off: Int): Position = {
    if (lines.length < 1)
      return Position(-1, -1)

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

  /**
    * Build line index for a given input sequence.
    *
    * The parser treats its input as a simple character sequence. In order to produce
    * positions with line numbers, we need to build an according line index.
    */
  private def buildLineIndex(s: CharSequence): Array[Int] = {
    val lines = new ArrayBuffer[Int]
    lines += 0
    for (i <- 0 until s.length)
      if (s.charAt(i) == '\n') lines += (i + 1)
    lines += s.length
    lines.toArray
  }

  /* Ignore whitespace and comments between parsers */
  override protected val whiteSpace = """(\s|--.*|(?m)\{-(\*(?!/)|[^*])*-\})+""".r

  private def parseTopLevel: Parser[AST] = {
    rep(makeDataDef | makeFunctionDef | makeFunctionSig) ^^ 
    { _.foldLeft(Program(Map(), Map(), Nil): AST)((z,f) => f(z)) }
  }
  
  private def makeDataDef: Parser[AST => AST] = dataDef ^^ { dataDef =>
    (ast: AST) => ast match {
      case prog: Program => prog.copy(dataDefs = dataDef :: prog.dataDefs)
    }
  }

  def makeFunctionDef: Parser[AST => AST] = (functionDef | binaryOpDef) ^^ { fun =>
    (a: AST) => a match {
      case prog: Program => prog.copy(functionDefs = updateMap(fun._1, fun._2, prog.functionDefs))
    }
  }

  def makeFunctionSig: Parser[AST => AST] = functionSig ^^ { sig =>
    (ast: AST) => ast match {
      case prog: Program => prog.copy(signatures = prog.signatures + sig)
    }
  }

  private def updateMap[T](s: String, t: T, m: Map[String, List[T]]) = m + (s -> (t :: m.get(s).getOrElse(Nil)))
  
  private def functionDef: Parser[(Var, FunctionDef)] = defLex ~> varRegex ~ rep(pat) ~ funEqLex ~ expr ^^@ {
    case (a, v ~ ps ~ _ ~ e) => (v, FunctionDef(ps, e, a))
  }

  private def binaryOpDef: Parser[(Var, FunctionDef)] = defLex ~> rep1(pat) ~ customOp ~ rep1(pat) ~ funEqLex ~ expr ^^@ {
    case (a, p1 ~ op ~ p2 ~ _ ~ e) => (op, FunctionDef(p1++p2, e, a))
  }


  private def dataDef: Parser[DataDef] = dataLex ~> typeRegex ~ rep(varRegex) ~ funEqLex ~ rep1sep(conDef, dataSepLex) ^^@
    { case (a, t ~ tvs ~ _ ~ cs) => DataDef(t, tvs, cs, a) }

  private def functionSig: Parser[Tuple2[Var, FunctionSig]] = funLex ~> varRegex ~ typeLex ~ parseType ^^@ { case (a, v ~ _ ~ t) => (v, FunctionSig(t, a)) }

  private def expr: Parser[Expr] = binop
  private def simpleexpr: Parser[Expr] = neg | app
  private def app: Parser[Expr] = chainl1(stmt, stmt <~ not(eqRegex), success(()) ^^ (_ => ((x: Expr, y: Expr) => App(x, y, mergeAttributes(x, y)))))
  private def stmt: Parser[Expr] = ( conditional
				   | lambda
				   | caseParser
				   | let
				   | parentheses
				   | exVar | exCon | string | num | char )

  private def conditional: Parser[Conditional] = ifLex ~> expr ~ thenLex ~ expr ~ elseLex ~ expr ^^@ {
    case (a, c ~ _ ~ e1 ~ _ ~ e2) => Conditional(c, e1, e2, a)
  }
  private def lambda: Parser[Lambda] = lambdaLex ~> rep(pat) ~ dotLex ~ expr ^^@ { case (a, p ~ _ ~ e) => Lambda(p, e, a) }
  private def caseParser: Parser[Case] = caseLex ~> expr ~ rep1(alt) ^^@ { case (a, e ~ as) => Case(e, as, a) }
  private def let: Parser[Let] = letLex ~> rep1(localDef) ~ inLex ~ expr ^^@ { case (a, lds ~ _ ~ e) => Let(lds, e, a) }
  private def parentheses: Parser[Expr] = "(" ~> expr <~ ")"
  private def string: Parser[ConstString] = """"(\\"|[^"])*"""".r ^^@ { (a, s: String) => ConstString(s.substring(1, s.length() - 1), a) }
  private def num: Parser[ConstInt] = """\d+""".r ^^@ { case (a, d) => ConstInt(d.toInt, a) }
  private def char: Parser[ConstChar] = """\'.\'""".r ^^@ { (a, s: String) => ConstChar(s.apply(1), a) }
  private def exVar: Parser[ExVar] = varRegex ^^@ { (a, s) => ExVar(s, a) }
  private def exCon: Parser[ExCon] = consRegex ^^@ { (a, s) => ExCon(s, a) }

  private def localDef: Parser[LetDef] = varRegex ~ funEqLex ~ expr ^^@ { case (a, v ~ _ ~ e) => LetDef(v, e, a) }

  private def alt: Parser[Alternative] = ofLex ~ ppat ~ thenLex ~ expr ^^@ { case (a, _ ~ p ~ _ ~ e) => Alternative(p, e, a) }

  private def neg: Parser[Expr] = subLex ~> expr ^^@ {
    case (a1, ConstInt(i, a2)) => ConstInt(-i, a1)
    case (a, e) => App(App(ExVar(mulLex), ConstInt(-1), a), e, a)
  }

  private def cons : Parser[ASTType] = consRegex ^^@ { (a, s) => TyExpr(s, Nil, a) }
  private def conElem : Parser[ASTType] = typeVar | cons | "(" ~> parseType <~ ")"
  private def conDef: Parser[ConstructorDef] = consRegex ~ rep(conElem) ^^@ { case (a, c ~ ts) => ConstructorDef(c, ts, a) }

  /*
   * Parse types.
   */
  private def parseType: Parser[ASTType] = baseType ~ (arrowLex ~> rep1sep(baseType, arrowLex)).? ^^@ { case (a, t1 ~ None) => t1 ; case (a, t1 ~ Some(ts)) => FunTy(t1::ts, a) }
  private def typeVar: Parser[TyVar] = varRegex ^^@ { (a, t) => TyVar(t, a) }
  private def typeExpr: Parser[TyExpr] = typeRegex ~ rep(parseType) ^^@ { case (a, t ~ ts) => TyExpr(t, ts, a) }
  private def baseType: Parser[ASTType] = typeVar | typeExpr | "(" ~> (parseType) <~ ")"

  /*
   * Parse Patterns.
   */
  private def ppat: Parser[Pattern] = patVar | patExpr | "(" ~> patExpr <~ ")"
  private def pat: Parser[Pattern] = patVar | patCons | "(" ~> patExpr <~ ")"
  private def patVar: Parser[PatternVar] = varRegex ^^@ { (a, s) => PatternVar(s, a) }
  private def patCons: Parser[Pattern] = consRegex ^^@ { (a, c) => PatternExpr(c, Nil, a) }
  private def patExpr: Parser[Pattern] = consRegex ~ rep(pat) ^^@ { case (a, c ~ pp) => PatternExpr(c, pp, a) }
  
  
  private def consRegex: Parser[String] = not(keyword) ~> """[A-Z][a-zA-Z0-9]*""".r ^^ { case s: String => s }
  private def typeRegex: Parser[String] = not(keyword) ~> """[A-Z][a-zA-Z0-9]*""".r ^^ { case s: String => s }
  private def varRegex: Parser[String] = """[a-z][a-zA-Z0-9]*""".r ^^ { case s: String => s }
  private def reservedOpsRegex: Parser[String] = """(\+s)|[\+\-\*><]|/|<=|==|/=|>=""".r ^^ { case s: String => s }
  private def binopRegex: Parser[ExVar] = (opRegex | reservedOpsRegex) ^^@ { (a, s) => ExVar(s, a) }
  private def opRegex: Parser[String] = """[!§%&/=\?\+\*#\-\<\>|][s!§%&/=\?\+\*#\-\<\>|]*""".r ^^ { case s: String => s }
  private def customOp: Parser[String] = Parser { in =>
    opRegex(in) match {
      case Success(t, in1) =>
        {
          if (predefinedOps.contains(t))
            Failure("Reserved operators are here not allowed", in1)
          else
            Success(t, in1)
        }
      case ns: NoSuccess => ns
    }
  }
  private def eqRegex: Parser[String] = """=(?![!§%&/=\?\+\*#\-:\<\>|])""".r ^^ { case s: String => s }
  private def keyword = keywords.mkString("", "|", "").r

  //Shunting-yard algorithm
  private def binop: Parser[Expr] = simpleexpr ~ rep(binopRegex ~ simpleexpr) ^^ {
    case x ~ xs =>
      var input = new Queue ++= (x :: (xs.flatMap({ case a ~ b => List(a, b) }))) //TODO
      val out: Stack[Expr] = new Stack
      val ops: Stack[Expr] = new Stack
      var isOp = false
      while (!input.isEmpty) {
        val o1 = input.dequeue
        if (isOp) {
          while (!ops.isEmpty && prec(o1) <= prec(ops.head)) {
            clearStack(out, ops)
          }
          ops.push(o1)
        } else {
          out.push(o1.asInstanceOf[Expr])
        }
        isOp = !isOp
      }
      while (!ops.isEmpty) clearStack(out, ops)
      if (out.size != 1) failure("OutputStack should have only one value")
      out.pop
  }

  private def clearStack(out: Stack[Expr], ops: Stack[Expr]) =
    {
      val o2 = ops.pop
      val a = out.pop
      val b = out.pop

      val att1 = mergeAttributes(b, o2)
      val att2 = mergeAttributes(b, a)
      out.push(App(App(o2, b, att1), a, att2))

    }

  private def prec(op: Any): Int = op match {
    case ExVar(`ltLex`, a)  => 1
    case ExVar(`leLex`, a)  => 1
    case ExVar(`eqLex`, a)  => 1
    case ExVar(`neLex`, a)  => 1
    case ExVar(`geLex`, a)  => 1
    case ExVar(`addLex`, a) => 2
    case ExVar(`strAdd`, a) => 2
    case ExVar(`subLex`, a) => 2
    case ExVar(`mulLex`, a) => 3
    case ExVar(`divLex`, a) => 3
    case _                  => 0
  }

  private def mergeAttributes(a: Expr, b: Expr): Attribute = {
    val aat = attribute(a)
    val bat = attribute(b)

    if (aat.isInstanceOf[AttributeImpl] && bat.isInstanceOf[AttributeImpl]) {
      val to = bat.asInstanceOf[AttributeImpl].location.asInstanceOf[FileLocation].to
      val from = aat.asInstanceOf[AttributeImpl].location.asInstanceOf[FileLocation].from
      val file = aat.asInstanceOf[AttributeImpl].location.asInstanceOf[FileLocation].file
      return new AttributeImpl(new FileLocation(file, from, to))
    } else
      return EmptyAttribute
  }


  /**
    * Parser which produces attributes, i.e., location hints, for each parsed token.
    */
  implicit class AttributedParser[T](p: Parser[T]) {

    def ^^@[U](f: (Attribute, T) => U): Parser[U] = Parser { in =>
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      val inwo = in.drop(start - offset)
      p(inwo) match {
        case Success(t, in1) => {
          val from = offsetToPosition(start)
          val to = offsetToPosition(in1.offset)
          val attr = AttributeImpl(FileLocation(fileName, from, to))
          Success(f(attr, t), in1)
        }
        case ns: NoSuccess => ns
      }
    }
  }

  implicit def regexAttributed(p: Regex): AttributedParser[String] = new AttributedParser(regex(p))
}


