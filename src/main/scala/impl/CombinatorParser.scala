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
 * */

package de.tuberlin.uebb.sl2.impl

import scala.language.implicitConversions
import de.tuberlin.uebb.sl2.modules._
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import scala.util.parsing.combinator.Parsers
import org.scalatest.time.Milliseconds
import scala.util.matching.Regex
import org.kiama.output.PrettyPrinter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

/**
 * Parser implementation module based on scala's combinators
 */
trait CombinatorParser extends RegexParsers with Parsers with Parser with Syntax with Errors with Lexic {

  def parseAst(in: String): Either[Error, AST] = {
    lines = buildLineIndex(in)
    try {
      parseAll(parseTopLevel, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case failure: NoSuccess => 
//          println("Parser Error: " + failure.toString)
          Left(convertError(failure)) // HERE IS WHERE THE EXCEPTIONS GET THROWN!
      }
    } catch {
      case err: Error => Left(err)
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left(ParseError("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString, AttributeImpl(NoLocation))) // TODO Use Attribute
    }
  }

  def parseExpr(in: String): Either[Error, Expr] = {
    try {
      parseAll(expr, new ParserString(in)) match {
        case Success(result, _) => Right(result)
        case failure: NoSuccess => Left(scala.sys.error(failure.msg))
      }
    } catch {
      case err: Error => Left(err)
      case e: Throwable =>
        val baos = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(baos))
        Left(ParseError("UNEXPECTED ERROR: " + e.getMessage() + "\n" + baos.toString, AttributeImpl(NoLocation))) // TODO: Fix this
    }
  }

  var fileName = ""

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

  //ignore whitespace and comments between parsers
  override protected val whiteSpace = """(\s|--.*|(?m)\{-(\*(?!/)|[^*])*-\})+""".r

  private def parseTopLevel: Parser[AST] = rep(makeImport | makeDataDef | makeFunctionDef | makeFunctionSig | makeFunctionDefExtern | makeError) ^^
    { _.foldLeft(emptyProgram: AST)((z, f) => f(z)) }

  private def makeImport: Parser[AST => AST] = (importDef | importExternDef | importDefError) ^^ { i =>
    (a: AST) =>
      a match {
        case m: Program => m.copy(imports = i :: m.imports)
      }
  }

  private def makeDataDef: Parser[AST => AST] = (dataDef | dataDefError) ^^ { d =>
    (a: AST) =>
      a match {
        case m: Program => m.copy(dataDefs = d :: m.dataDefs)
      }
  }

  def makeFunctionDef: Parser[AST => AST] = (functionDef | binaryOpDef | funDefError) ^^ { x =>
    (a: AST) =>
      a match {
        case m: Program => m.copy(functionDefs = updateMap(x._1, x._2, m.functionDefs))
      }
  }

  def makeFunctionSig: Parser[AST => AST] = (functionSig | funSigError) ^^ { x =>
    (a: AST) =>
      a match {
        case m: Program => m.copy(signatures = m.signatures + x)
      }
  }

  def makeFunctionDefExtern: Parser[AST => AST] = (functionDefExtern | funDefExternError) ^^ { x =>
    (a: AST) =>
      a match {
        case m: Program => m.copy(functionDefsExtern = m.functionDefsExtern + x)
      }
  }

  def makeError: Parser[AST => AST] = anyToken ^^@ {
      case (a, found) => throw ParseError("unexpected token: " + quote(found) + "; expected top level definition", a)
  }

  private def updateMap[T](s: String, t: T, m: Map[String, List[T]]) = m + (s -> (t :: m.get(s).getOrElse(Nil))) 

  private def importDef: Parser[Import] = 
    importLex ~> string ~ asLex ~ checkedModuleIde ^^@ {
      case (a, name ~ _ ~ mod) => QualifiedImport(name.value, mod, a)
    }

  private def importExternDef: Parser[Import] =
    importLex ~ externLex ~> string ^^@ {
      case (a, name) => ExternImport(name.value, a)
    }

  private def functionDef: Parser[(VarName, FunctionDef)] =
    defLex ~> varRegex ~ rep(pat) ~ not(opRegex) ~ expect(funEqLex) ~ expr ^^@ {
      case (a, v ~ ps ~ _ ~ _ ~ e) => (v, FunctionDef(ps, e, a))
    }

  private def binaryOpDef: Parser[(VarName, FunctionDef)] =
    defLex ~> rep1(pat) ~ opRegex ~ rep1(pat) ~ expect(funEqLex) ~ expr ^^@ {
      case (a, p1 ~ op ~ p2 ~ _ ~ e) => (op, FunctionDef(p1 ++ p2, e, a))
    }

  private def functionDefExtern: Parser[(VarName, FunctionDefExtern)] =
    defLex ~ externLex ~> (varRegex|opRegex) ~ expect(funEqLex) ~ jsRegex ^^@ {
      case (a, v ~ _ ~ js) => (v, FunctionDefExtern(js, a))
    }

  private def dataDef: Parser[DataDef] = (
      publicLex ~> dataLex ~> externLex ~> checkedTypeIde ~ rep(varRegex) ^^@
      { case (a, t ~ tvs) => DataDef(t, tvs, List(), PublicModifier, a) }
    | dataLex ~> externLex ~> checkedTypeIde ~ rep(varRegex) ^^@
      { case (a, t ~ tvs) => DataDef(t, tvs, List(), DefaultModifier, a) }
    | publicLex ~> dataLex ~> checkedTypeIde ~ rep(varRegex) ~ expect(funEqLex) ~ rep1sep(conDef, dataSepLex) ^^@
      { case (a, t ~ tvs ~ _ ~ cs) => DataDef(t, tvs, cs, PublicModifier, a) }
    | dataLex ~> checkedTypeIde ~ rep(varRegex) ~ expect(funEqLex) ~ rep1sep(conDef, dataSepLex) ^^@
      { case (a, t ~ tvs ~ _ ~ cs) => DataDef(t, tvs, cs, DefaultModifier, a) })


  private def functionSig: Parser[Tuple2[VarName, FunctionSig]] = (
      publicLex ~> funLex ~> expect(varRegex|opRegex, "variable or operator name") ~ expect(typeLex) ~ parseType ^^@ { case (a, v ~ _ ~ t) => (v, FunctionSig(t, PublicModifier, a)) }
    | funLex ~> expect(varRegex|opRegex, "variable or operator name") ~ expect(typeLex) ~ parseType ^^@ { case (a, v ~ _ ~ t) => (v, FunctionSig(t, DefaultModifier, a)) } )

  private def expr: Parser[Expr] = binop
  private def simpleexpr: Parser[Expr] = app
  private def app: Parser[Expr] = chainl1(stmt, stmt <~ not(eqRegex), success(()) ^^ (_ => ((x: Expr, y: Expr) => App(x, y, mergeAttributes(x, y)))))
  private def stmt: Parser[Expr] = (
    conditional
    | lambda
    | caseParser
    | let
    | javaScript
    | parentheses
    | exVar | exCon | string | real | num | char | badStmt)

  private def conditional: Parser[Conditional] = ifLex ~> expr ~ expect(thenLex) ~ expr ~ expect(elseLex) ~ expr ^^@ { case (a, c ~ _ ~ e1 ~ _ ~ e2) => Conditional(c, e1, e2, a) }
  private def lambda: Parser[Lambda] = lambdaLex ~> rep(pat) ~ dotLex ~ expr ^^@ { case (a, p ~ _ ~ e) => Lambda(p, e, a) }
  private def caseParser: Parser[Case] = caseLex ~> expr ~ rep1(alt) ^^@ { case (a, e ~ as) => Case(e, as, a) }
  private def let: Parser[Let] = letLex ~> rep1(localDef) ~ expect(inLex) ~ expr ^^@ { case (a, lds ~ _ ~ e) => Let(lds, e, a) }
  private def javaScript: Parser[Expr] = ((jsOpenLex ~> """(?:(?!\|\}).)*""".r <~ jsCloseLex) ~ (typeLex ~> parseType?)) ^^@ { case (a, s ~ t) => JavaScript(s, t, a) }
  private def parentheses: Parser[Expr] = "(" ~> expr <~ closeBracket(")")
  private def string: Parser[ConstString] = """"(\\"|[^"])*"""".r ^^@ { (a, s: String) => ConstString(s.substring(1, s.length() - 1), a) }
  private def num: Parser[ConstInt] = """-?\d+""".r ^^@ { case (a, d) => ConstInt(d.toInt, a) }
  private def real: Parser[ConstReal] = """-?(\d+\.\d*|\.\d+)([Ee]-?\d+)?""".r ^^@ {
    case (a, d) => ConstReal(d.toDouble, a)
  }
  private def char: Parser[ConstChar] = """\'.\'""".r ^^@ { (a, s: String) => ConstChar(s.apply(1), a) }
  private def exVar: Parser[ExVar] = qualVar | unqualVar
  private def exCon: Parser[ExCon] = qualCon | unqualCon

  private def unqualVar: Parser[ExVar] = varRegex ^^@ { (a, s) => ExVar(Syntax.Var(s), a) }
  private def unqualCon: Parser[ExCon] = consRegex ~ not(".") ^^@ { case (a, s~_) => ExCon(Syntax.ConVar(s), a) }

  private def qualVar: Parser[ExVar] = moduleRegex ~ "." ~ varRegex  ^^@ { case (a, m~_~s) => ExVar(Syntax.Var(s,m), a) }
  private def qualCon: Parser[ExCon] = moduleRegex ~ "." ~ consRegex  ^^@ { case (a, m~_~s) => ExCon(Syntax.ConVar(s,m), a) }

  private def localDef: Parser[LetDef] = varRegex ~ funEqLex ~ expr ^^@ { case (a, v ~ _ ~ e) => LetDef(v, e, a) }

  private def alt: Parser[Alternative] = ofLex ~ ppat ~ expect(thenLex) ~ expr ^^@ { case (a, _ ~ p ~ _ ~ e) => Alternative(p, e, a) }

  private def cons: Parser[ASTType] = consRegex ^^@ { (a, s) => TyExpr(Syntax.TConVar(s), Nil, a) }
  private def qualCons: Parser[ASTType] = moduleRegex ~ "." ~ checkedConsIde ^^@ { case (a, m~_~s) => TyExpr(Syntax.TConVar(s,m), Nil, a) }
  private def conElem: Parser[ASTType] = 
    typeVar | qualCons | cons | "(" ~> parseType <~ closeBracket(")") //| not(dataSepLex) ~> unexpectedIn("data constructor definition")
  private def conDef: Parser[ConstructorDef] = consRegex ~ rep(conElem) ^^@ { case (a, c ~ ts) => ConstructorDef(c, ts, a) }

  //  //Parse Types
  private def parseType: Parser[ASTType] =
    baseType ~ (arrowLex ~> rep1sep(baseType, arrowLex)).? ^^@ {
      case (a, t1 ~ None) => t1; case (a, t1 ~ Some(ts)) => FunTy(t1 :: ts, a)
    }
  private def typeVar: Parser[TyVar] = varRegex ^^@ {
    (a, t) => TyVar(t, a)
  }
  private def typeExpr: Parser[TyExpr] =
    typeCon ~ rep(baseType) ^^@ {
      case (a, t ~ ts) => TyExpr(t, ts, a)
    }
  private def baseType: Parser[ASTType] = typeVar | typeExpr | "(" ~> (parseType) <~ closeBracket(")")

  private def typeArg: Parser[ASTType] = simpleType | typeVar | "(" ~> parseType <~ closeBracket(")")

  private def simpleType: Parser[ASTType] = typeCon ^^@ {
    (a, t) => TyExpr(t, List(), a)
  }

  private def typeCon: Parser[Syntax.TConVar] = qualTCon | unqualTCon

  private def qualTCon: Parser[Syntax.TConVar] = moduleRegex ~ "." ~ typeRegex ^^ {
    case m~_~t => Syntax.TConVar(t, m)
  }
  private def unqualTCon: Parser[Syntax.TConVar] = typeRegex ~ not(".") ^^ { case t~_ => Syntax.TConVar(t)}

  //Parse Pattern
  private def ppat: Parser[Pattern] = patVar | patQualExpr | patExpr
  private def pat: Parser[Pattern] = patVar | patQualCons | patCons | "(" ~> (patQualExpr | patExpr) <~ closeBracket(")")
  private def patVar: Parser[PatternVar] = varRegex ^^@ {
    (a, s) => PatternVar(s, a)
  }
  private def patCons: Parser[Pattern] = consRegex ^^@ {
    (a, c) => PatternExpr(Syntax.ConVar(c), Nil, a)
  }
  private def patQualCons: Parser[Pattern] = moduleRegex ~ "." ~ consRegex ^^@ {
    case (a, m ~ _ ~ c) => PatternExpr(Syntax.ConVar(c, m), Nil, a)
  }
  private def patExpr: Parser[Pattern] = consRegex ~ rep(pat) ^^@ {
    case (a, c ~ pp) => PatternExpr(Syntax.ConVar(c), pp, a)
  }
  private def patQualExpr: Parser[Pattern] = moduleRegex ~ "." ~ consRegex ~ rep(pat) ^^@ {
    case (a, m ~ _ ~ c ~ pp) => PatternExpr(Syntax.ConVar(c, m), pp, a)
  }

  private def consRegex: Parser[String] = not(keyword) ~> """[A-Z][a-zA-Z0-9]*""".r ^^ { case s: String => s }
  private def typeRegex: Parser[String] = not(keyword) ~> """[A-Z][a-zA-Z0-9]*""".r ^^ { case s: String => s }
  private def moduleRegex: Parser[String] = not(keyword) ~> """[A-Z][a-zA-Z0-9]*""".r ^^ { case s: String => s }
  private def varRegex: Parser[String] = """[a-z][a-zA-Z0-9]*""".r ^^ { case s: String => s }
  private def opRegex: Parser[String] = not(eqRegex) ~> """[!§%&/=\?\+\*#\-\<\>|]+""".r ^^ { case s: String => s }
  private def eqRegex: Parser[String] = """=(?![!§%&/=\?\+\*#\-:\<\>|])""".r ^^ { case s: String => s }
  private def keyword: Parser[String] = keywords.mkString("", "|", "").r
  private def jsRegex = jsOpenLex ~> """(?:(?!\|\}).|\n|\r)*""".r <~ jsCloseLex ^^ { case s: String => s }
  private def anyToken = keyword | consRegex | varRegex | eqRegex | jsOpenLex | jsCloseLex | """^[ \t\n]""".r

  //Qualified things
  private def unqualBinop: Parser[ExVar] = 
    opRegex ^^@ { (a, s) => ExVar(Syntax.Var(s), a)}

  private def qualBinop: Parser[ExVar] =
    moduleRegex ~ "." ~ opRegex ^^@ {case (a, m ~ _ ~ s) => ExVar(Syntax.Var(s, m), a)} 

  //Shunting-yard algorithm
  private def binop: Parser[Expr] = simpleexpr ~ rep((unqualBinop | qualBinop) ~ simpleexpr) ^^ {
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

  // Parse errors
  private def closeBracket(bracket: String): Parser[String] =
    bracket | bracketError(bracket) ^^ {case s: String => s}

  private def bracketError(expected: String): Parser[String] = 
    (")" | "]" | "}" | jsCloseLex | funLex | defLex | importLex | dataLex | publicLex | externLex | anyToken) ^^@ {
      case (a, found) => throw ParseError("unbalanced parentheses: " + quote(expected) + " expected but " + quote(found) + " found.", a)
    }

  private def expect(expected: String): Parser[String] =
    expected | wrongToken(expected)

  private def expect(parser: Parser[String], description: String): Parser[String] =
    parser | wrongTokenType(description) ^^ {case s: String => s}

  private def wrongToken(expected: String): Parser[String] = 
    anyToken ^^@ {
      case (a, found) => throw ParseError("unexpected token: "+ quote(expected) + " expected but " + quote(found) + " found.", a)
    }

  private def checkedVarIde: Parser[String] =
    varRegex | wrongTokenType("lower case identifier")

  private def checkedModuleIde: Parser[String] =
    moduleRegex | wrongTokenType("module identifier") ^^ {case s: String => s}

  private def checkedTypeIde: Parser[String] =
    typeRegex | wrongTokenType("type identifier") ^^ {case s: String => s}

  private def checkedConsIde: Parser[String] =
    consRegex | wrongTokenType("constructor identifier") ^^ {case s: String => s}

  private def wrongTokenType(expected: String): Parser[String] = anyToken ^^@ {
      case (a, found) => throw ParseError(expected + " expected but " + quote(found) + " found.", a)
    }

  private def importDefError: Parser[Import] = 
    importLex ~> anyToken ^^@ {
      case (a, found) => throw ParseError("malformed import declaration: " + quote(found) + " unexpected.", a)
    }

  private def dataDefError: Parser[DataDef] =
    (publicLex?) ~> dataLex ~> anyToken ^^@ {
      case (a, found) => throw ParseError("malformed data declaration: " + quote(found) + " unexpected.", a)
    }

  private def funSigError: Parser[Tuple2[VarName, FunctionSig]] =
    (publicLex?) ~> funLex ~> anyToken ^^@ {
      case (a, found) => throw ParseError("malformed function signature: " + quote(found) + " unexpected.", a)
    }

  private def funDefError: Parser[(VarName, FunctionDef)] =
    defLex ~> not(externLex) ~> anyToken ^^@ {
      case (a, found) => throw ParseError("malformed function definition: " + quote(found) + " unexpected.", a)
    }

  private def funDefExternError: Parser[(VarName, FunctionDefExtern)] =
    defLex ~> externLex ~> anyToken ^^@ {
      case (a, found) => throw ParseError("malformed extern function definition: " + quote(found) + " unexpected.", a)
    }

  private def badStmt: Parser[Expr] = "." ~> anyToken ^^@ {
    case (a, found) => throw ParseError("unexpected qualification operator before " + quote(found) + ".", a)
  } // TODO add here!

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
    case ExVar(Syntax.Var(`gtLex`, _), a) => 1
    case ExVar(Syntax.Var(`ltLex`, _), a) => 1
    case ExVar(Syntax.Var(`leLex`, _), a) => 1
    case ExVar(Syntax.Var(`eqLex`, _), a) => 1
    case ExVar(Syntax.Var(`neLex`, _), a) => 1
    case ExVar(Syntax.Var(`geLex`, _), a) => 1
    case ExVar(Syntax.Var(`bindLex`, _), a) => 1
    case ExVar(Syntax.Var(`bindNRLex`, _), a) => 1
    case ExVar(Syntax.Var(`addLex`, _), a) => 2
    case ExVar(Syntax.Var(`subLex`, _), a) => 2
    case ExVar(Syntax.Var(`mulLex`, _), a) => 3
    case ExVar(Syntax.Var(`divLex`, _), a) => 3
    case _ => 0
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

  private def convertError(ns: NoSuccess) = ns match {
    case NoSuccess(msg, in1) => 
      val pos = offsetToPosition(in1.offset)
      val attr = AttributeImpl(FileLocation(fileName, pos, pos))
      throw ParseError(msg, attr)
  }

  private implicit def parser2Attributed[T](p: Parser[T]): AttributedParser[T] = new AttributedParser(p)
  private implicit def regexAttributed(p: Regex): AttributedParser[String] = new AttributedParser(regex(p))

  private class AttributedParser[T](p: Parser[T]) {

    def ^^@[U](f: (Attribute, T) => U): Parser[U] = Parser { in =>
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      val inwo = in.drop(start - offset)
      p(inwo) match {
        case Success(t, in1) =>
          {
            val from = offsetToPosition(start)
            val to = offsetToPosition(in1.offset)
            val att = AttributeImpl(FileLocation(fileName, from, to))
            Success(f(att, t), in1)
          }
/*
        case Error(msg, in1) => 
          {
            val from = offsetToPosition(start)
            val to = offsetToPosition(in1.offset)
            val att = AttributeImpl(FileLocation(fileName, from, to))
            throw ParseError("From CombinatorParser: " + msg, att)
          }
 */
        case ns: NoSuccess => ns
      }
    }
  }

}


