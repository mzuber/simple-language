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

package de.tuberlin.uebb.sl2.tests.specs

import org.scalatest.matchers._
import org.scalatest.{ FunSpec, Inside }
import scala.language.implicitConversions
import de.tuberlin.uebb.sl2.modules._

trait ParserSpec extends FunSpec with Inside with ShouldMatchers {
  this: Parser with Syntax with Lexic with Errors =>

  case class ParseCommand[T](s: String, cmd: String => Either[Error, T])

  implicit class ParsingInput(s: String) {
    def as() = Parsing(s)
  }

  def functionDef2Modul(n: VarName, d: List[FunctionDef]): AST = Program(List(), Map.empty, Map(n -> d), Map.empty, Nil)
  def dataDef2Modul(d: DataDef): AST = Program(List(), Map.empty, Map.empty, Map.empty, List(d))
  def functionDefs2Modul(d: Map[VarName, List[FunctionDef]]): AST = Program(List(), Map.empty, d, Map.empty, Nil)

  def exVar(name: String) = ExVar(Syntax.Var(name))

  case class Parsing(s: String) {
    def expr() = ParseCommand(s, parseExpr)
    def ast() = ParseCommand(s, parseAst)
  }

  case class ParseMatcher[T](expected: T) extends Matcher[ParseCommand[_ >: T]] {

    def apply(c: ParseCommand[_ >: T]) = {

      val res = c.cmd(c.s)

      val failureMessageSuffix = (for (r <- res.right) yield "'" + c.s + "' resulted in " + r + " which did not equal " + expected).fold("failed: %s".format(_), _.toString)

      val negatedFailureMessageSuffix =
        "'" + c.s + "' yielded " + expected

      val equals = (for (r <- res.right) yield r == expected).right.toOption.getOrElse(false)

      MatchResult(
        equals,
        "Parsing " + failureMessageSuffix,
        "Parsing " + negatedFailureMessageSuffix,
        "Parsing " + failureMessageSuffix,
        "Parsing " + negatedFailureMessageSuffix)
    }
  }

  def testedImplementationName(): String

  def parse[T](a: T) = ParseMatcher(a)

  describe(testedImplementationName() + " Test case 1: Basic tests") {
    it("Should parse Integer literals") {
      "42".as.expr should parse(ConstInt(42))
    }

    it("Should parse negative Integer literals") {
      "-42".as.expr should parse(ConstInt(-42))
    }

    it("Should parse real literals") {
      "1.0".as.expr should parse(ConstReal(1.0))
    }

    it("Should parse other real literals") {
      ".1".as.expr should parse(ConstReal(.1))
    }

    it("Should parse negative real literals") {
      "-1.0".as.expr should parse(ConstReal(-1.0))
    }

    it("Should parse negative real literals with exponent") {
      "-0.5E2".as.expr should parse(ConstReal(-0.5e2))
    }

    it("Should parse negative real literals with negative exponents") {
      "-0.5E-2".as.expr should parse(ConstReal(-0.5e-2))
    }

    it("Should parse Character literals") {
      "'c'".as.expr should parse(ConstChar('c'))
    }

    it("Should parse string-literals correctly") {
      """f "x" x "x"""".as.expr should parse(App(App(App(exVar("f"), ConstString("x")), exVar("x")), ConstString("x")))
    }

    it("Should parse variables") {
      "x".as.expr should parse(ExVar(Syntax.Var("x")))
    }
    it("Should parse variables with parentheses") {
      "(x)".as.expr should parse(ExVar(Syntax.Var("x")))
    }

    it("Should parse multi-letter variables") {
      "foo".as.expr should parse(ExVar(Syntax.Var("foo")))
    }

    it("Should parse multi-letter constructors") {
      "Foo".as.expr should parse(ExCon(Syntax.ConVar("Foo")))
    }

    it("Should parse string-literals") {
      """"TUB"""".as.expr should parse(ConstString("TUB"))
    }

    it("Should parse Integer literals followed by a comment") {
      "42 {-comment\nnewline-}".as.expr should parse(ConstInt(42))
    }

    it("Should parse Integer literals followed by a one line comment") {
      "42 --comment".as.expr should parse(ConstInt(42))
    }

    it("Should parse Character literals followed by a comment") {
      "'c' {-comment-}".as.expr should parse(ConstChar('c'))
    }

    it("Should parse variables followed by a comment") {
      "x  {-comment-}".as.expr should parse(ExVar(Syntax.Var("x")))
    }

    it("Should parse constructors followed by a comment") {
      "Foo {-comment-}".as.expr should parse(ExCon(Syntax.ConVar("Foo")))
    }

    it("Should parse string-literals followed by a comment") {
      """"TUB" {-comment-}""".as.expr should parse(ConstString("TUB"))
    }

    it("Should parse simple function definition") {
      "DEF f x = x".as.ast should parse(functionDef2Modul(("f"), List(FunctionDef(List(patX), varX))))
    }

    it("Should parse binary opertor definition") {
      "DEF x ++ y = x".as.ast should parse(functionDef2Modul(("++"), List(FunctionDef(List(patX, patY), varX))))
    }

    it("Should parse inline JavaScript") {
      """{|alert("Hello!")|}""".as.expr should parse(JavaScript("""alert("Hello!")""", None))
    }

    it("Should parse inline JavaScript with type annotation") {
      """{|33|} : (DOM Int)""".as.expr should parse(JavaScript("""33""", Some(TyExpr(Syntax.TConVar("DOM"), TyExpr(Syntax.TConVar("Int"), Nil) :: Nil))))
    }

    it("Should parse function with two definitions") {
      """DEF f Nil = 0 
         DEF f (Cons a b) = 1""".as.ast should parse((functionDef2Modul(("f"), List(
        FunctionDef(List(PatternExpr(Syntax.ConVar("Cons"), List(PatternVar("a"), PatternVar("b")))), ConstInt(1)),
        FunctionDef(List(PatternExpr(Syntax.ConVar("Nil"), Nil)), ConstInt(0))))))
    }
    
    it("Should parse external function definition") {
      "DEF EXTERN f = {|myJavaScriptFun|}".as.ast should parse(
          Program(List(), Map.empty, Map.empty, Map("f" -> FunctionDefExtern("myJavaScriptFun")), Nil))
    }
  }

  describe(testedImplementationName() + " Test case 2: Function application") {
    it("Should parse a function application") {
      "g x y".as.expr should parse(App(App(exVar("g"), exVar("x")), exVar("y")))
    }

    it("Should parse application cut off by a comment") {
      "f {-42-} x".as.expr should parse(App(exVar("f"), exVar("x")))
    }

    it("Should parse custom binary operator application") {
      "x +++ y".as.expr should parse(App(App(exVar("+++"), exVar("x")), exVar("y")))
    }

    it("Should invert application order in case of operators") {
      "x - y".as.expr should parse(App(App(exVar(subLex), exVar("x")), exVar("y")))
    }

    it("Should parse a function application mixed with an operator") {
      "g x y + z".as.expr should parse(App(App(exVar(addLex), App(App(exVar("g"), exVar("x")), exVar("y"))), exVar("z")))
    }

    it("Should parse binary minus in arithmetic expression") {
      "x - 1".as.expr should parse(App(App(exVar(subLex), exVar("x")), ConstInt(1)))
    }

    it("Should parse string concatenation") {
      "x Str.++ y".as.expr should parse(App(App(ExVar(Syntax.Var("++", "Str")), exVar("x")), exVar("y")))
    }

    it("Should parse real division") {
      "x Real./ y".as.expr should parse(App(App(ExVar(Syntax.Var("/", "Real")), exVar("x")), exVar("y")))
    }

    it("Should parse real multiplication") {
      "x Real.* y".as.expr should parse(App(App(ExVar(Syntax.Var("*", "Real")), exVar("x")), exVar("y")))
    }

    it("Should parse real addition") {
      "x Real.+ y".as.expr should parse(App(App(ExVar(Syntax.Var("+", "Real")), exVar("x")), exVar("y")))
    }

    it("Should parse real subtraction") {
      "x Real.- y".as.expr should parse(App(App(ExVar(Syntax.Var("-", "Real")), exVar("x")), exVar("y")))
    }
  }

  describe(testedImplementationName() + " Test case 3: operator precedence") {

    it("Should bind rhs multiplication over lhs comparison") {
      "x == y * z".as.expr should parse(App(App(exVar(eqLex), exVar("x")), App(App(exVar(mulLex), exVar("y")), exVar("z"))))
    }

    it("Should bind rhs multiplication over lhs addition") {
      "x + y * z".as.expr should parse(App(App(exVar(addLex), exVar("x")), App(App(exVar(mulLex), exVar("y")), exVar("z"))))
    }

    it("Should bind lhs multiplication over rhs subtraction for reals") {
      "x Real.- y Real.* z".as.expr should parse(
          App(App(ExVar(Syntax.Var("-", "Real")), exVar("x")), App(App(ExVar(Syntax.Var("*", "Real")), exVar("y")), exVar("z"))))
    }

    it("Should bind rhs division over lhs subtraction") {
      "x - y / z".as.expr should parse(App(App(exVar(subLex), exVar("x")), App(App(exVar(divLex), exVar("y")), exVar("z"))))
    }

    it("Should bind lhs multiplication over rhs addition") {
      "x * y + z".as.expr should parse(App(App(exVar(addLex), App(App(exVar(mulLex), exVar("x")), exVar("y"))), exVar("z")))
    }

    it("Should bind lhs division over lhs subtraction") {
      "x / y - z".as.expr should parse(App(App(exVar(subLex), App(App(exVar(divLex), exVar("x")), exVar("y"))), exVar("z")))
    }

    it("Should bind custom ops lower as addition") {
      "1 + 3 +++ 2".as.expr should parse(App(App(exVar("+++"), App(App(exVar("+"), ConstInt(1)), ConstInt(3))), ConstInt(2)))
    }

    it("Should bind custom ops lower as multiplication") {
      "1 * 3 +++ 2".as.expr should parse(App(App(exVar("+++"), App(App(exVar("*"), ConstInt(1)), ConstInt(3))), ConstInt(2)))
    }

    it("Should bind custom ops lower as comparison") {
      "1 == 3 +++ 2".as.expr should parse(App(App(exVar("+++"), App(App(exVar("=="), ConstInt(1)), ConstInt(3))), ConstInt(2)))
    }
  }

  describe(testedImplementationName() + " Test case 4: IF / THEN / ELSE") {

    it("Should parse a simple IF-THEN-ELSE") {
      "IF x THEN 1 ELSE 10".as.expr should parse(Conditional(varX, ConstInt(1), ConstInt(10)))
    }

    it("Should parse a nested IF-THEN-ELSE") {
      "IF IF 1 THEN x ELSE y THEN 1 ELSE 10".as.expr should parse(Conditional(Conditional(ConstInt(1), exVar("x"), exVar("y")), ConstInt(1), ConstInt(10)))
    }
  }

  describe(testedImplementationName() + " Test case 5: Pattern matching") {
    it("Should parse patter matching in function definition") {
      ("DEF map g Nil = Nil\n" + "DEF map g (Cons x y) = Cons (g x) (map g y)").as.ast should parse(functionDefs2Modul(
        Map(
          strMap -> List(
            FunctionDef(
              List(patG, PatternExpr(Syntax.ConVar(consLex), List(patX, patY))),
              App(App(ExCon(Syntax.ConVar(consLex)), App(varG, varX)), App(App(varMap, varG), varY))),
            FunctionDef(List(patG, PatternExpr(Syntax.ConVar(nilLex), Nil)), ExCon(Syntax.ConVar(nilLex)))))))
    }

    it("Should parse patter matching in function definition with variable") {
      ("DEF map Nil g = Nil\n").as.ast should parse(functionDefs2Modul(
        Map(
          strMap -> List(
            FunctionDef(List(PatternExpr(Syntax.ConVar(nilLex), Nil), patG), ExCon(Syntax.ConVar(nilLex)))))))
    }

  }

  describe(testedImplementationName() + " Test case 6: LET-expressions") {

    it("Should parse a let-expression with one variable") {
      "LET x = 1 IN x".as.expr should parse(Let(List(LetDef("x", ConstInt(1))), exVar("x")))
    }

    it("Should parse a let-expression with multiple variables") {
      "LET x = 10 y = 20 IN x".as.expr should parse(Let(List(LetDef("x", ConstInt(10)), LetDef("y", ConstInt(20))), exVar("x")))
    }

    it("Should parse a let-expression with operater beginning with =") {
      "LET x = 10 == 3 IN x".as.expr should parse(Let(List(LetDef("x", App(App(exVar("=="), ConstInt(10)), ConstInt(3)))), exVar("x")))
    }
  }

  describe(testedImplementationName() + " Test case 7: Lambda-expressions") {

    it("Should parse the identity function") {
      ("\\ x . x").as.expr should parse(Lambda(List(PatternVar("x")), exVar("x")))
    }

    it("Should parse " + lambdaLex + " x y . y") {
      ("\\ x y . y").as.expr should parse(Lambda(List(PatternVar("x"), PatternVar("y")), exVar("y")))
    }

    it("Should parse " + lambdaLex + " Nil . 1") {
      ("\\ Nil . 1").as.expr should parse(Lambda(List(PatternExpr(Syntax.ConVar(nilLex), Nil)), ConstInt(1)))
    }

  }

  describe(testedImplementationName() + " Test case 8: Case-expressions") {
    val str = """
    |CASE y 
    |  OF Nil THEN Nil
    |  OF Cons x xs THEN Cons (f x) (map f xs)    
    |""".stripMargin.trim

    def appCons(ft: App, rt: App) = App(App(ExCon(Syntax.ConVar(consLex)), ft), rt)

    it("Should parse list pattern matching") {
      str.as.expr should parse(
        Case(exVar("y"),
          List(Alternative(PatternExpr(Syntax.ConVar(nilLex), Nil), ExCon(Syntax.ConVar(nilLex))),
            Alternative(PatternExpr(Syntax.ConVar(consLex), List(PatternVar("x"), PatternVar("xs"))),
              appCons(App(exVar("f"), exVar("x")), App(App(exVar("map"), exVar("f")), exVar("xs")))))))
    }

    val much = 10000 // increase for combinator blowup
    it("Should parse quite large pattern matching expressions") {
      inside(parseExpr("CASE y " + ("OF Nil THEN Nil " * much))) {
        case Right(Case(ExVar(Syntax.Var(y, _), _), alternatives, _)) => {
          y should be("y")
          alternatives should have length (much)
        }
      }
    }
  }

  describe(testedImplementationName() + " Test case 9: Data type definitions") {
    it("Should parse very simple wrapper data type definition") {
      "DATA IntWrapper = IntWrapper Int".as.ast should parse(dataDef2Modul(DataDef("IntWrapper",
        List(),
        List(ConstructorDef("IntWrapper", List(TyExpr(Syntax.TConVar("Int"), List())))))))
    }
    
    it("Should parse very simple wrapper data type definition with qualified names") {
      "DATA IntWrapper = IntWrapper Pre.Int".as.ast should parse(dataDef2Modul(DataDef("IntWrapper",
        List(),
        List(ConstructorDef("IntWrapper", List(TyExpr(Syntax.TConVar("Int", "Pre"), List())))))))
    }
    
    it("Should parse simple generic data type definition") {
      "DATA Product x y = Product x y".as.ast should parse(dataDef2Modul(DataDef(strProduct,
        List(strX, strY),
        List(ConstructorDef(strProduct, List(TyVar(strX), TyVar(strY)))))))
    }
    
    it("Should parse wrapper data type definition with qualified names") {
      "DATA ListWrapper x = ListWrapper (List.List x)".as.ast should parse(dataDef2Modul(DataDef("ListWrapper",
        List(strX),
        List(ConstructorDef("ListWrapper", List(TyExpr(Syntax.TConVar("List", "List"), List(TyVar(strX)))))))))
    }
    
    it("Should parse simple public data type definition") {
      "PUBLIC DATA Product x y = Product x y".as.ast should parse(dataDef2Modul(DataDef(strProduct,
        List(strX, strY),
        List(ConstructorDef(strProduct, List(TyVar(strX), TyVar(strY)))), PublicModifier)))
    }

    it("Should parse algebraic data type definition") {
      "DATA List x = Cons x (List x) | Nil".as.ast should parse(dataDef2Modul(DataDef(strList, List(strX),
        List(
          ConstructorDef(consLex, List(
            TyVar(strX),
            TyExpr(Syntax.TConVar(strList), List(
              TyVar(strX))))),
          ConstructorDef(nilLex, Nil)))))
    }

    it("Should parse tree data type definition without argument") {
      "DATA Tree =  Node Tree Tree | Leaf Int ".as.ast should parse(dataDef2Modul(DataDef("Tree", Nil,
        List(
          ConstructorDef("Node", List(
            TyExpr(Syntax.TConVar("Tree"), Nil), TyExpr(Syntax.TConVar("Tree"), Nil))),
          ConstructorDef("Leaf", List(TyExpr(Syntax.TConVar("Int"), Nil)))))))
    }

    it("Should parse tree data type definition with argument") {
      "DATA Tree a =  Node (Tree a) (Tree a) | Leaf a ".as.ast should parse(dataDef2Modul(DataDef("Tree", List("a"),
        List(
          ConstructorDef("Node", List(
            TyExpr(Syntax.TConVar("Tree"), List(TyVar("a"))), TyExpr(Syntax.TConVar("Tree"), List(TyVar("a"))))),
          ConstructorDef("Leaf", List(TyVar("a")))))))
    }

    it("Should parse function types inside DATA defs") {
      "DATA Fun = IntFun (Int -> Int)".as.ast should parse(dataDef2Modul(
        DataDef("Fun", Nil,
          ConstructorDef("IntFun", FunTy(TyExpr(Syntax.TConVar("Int"), Nil) :: TyExpr(Syntax.TConVar("Int"), Nil) :: Nil) :: Nil) :: Nil)))
    }

  }

  describe(testedImplementationName() + " Test case 10 : Function signatures") {
    it("Should parse simple function signature") {
      "FUN x : (Int) -> (Int)".as.ast should
        parse(Program(
          List(),
          Map((strX) -> FunctionSig(FunTy(List(TyExpr(Syntax.TConVar("Int"), Nil), TyExpr(Syntax.TConVar("Int"), Nil))))),
          Map.empty, Map.empty, Nil))
    }
    
    it("Should parse simple public function signature") {
      "PUBLIC FUN x : (Int) -> (Int)".as.ast should
        parse(Program(
          List(),
          Map((strX) -> FunctionSig(FunTy(List(TyExpr(Syntax.TConVar("Int"), Nil), TyExpr(Syntax.TConVar("Int"), Nil))), PublicModifier)),
          Map.empty, Map.empty, Nil))
    }

    it("Should parse simple constant signature") {
      "FUN x : Int".as.ast should
        parse(Program(
          List(),
          Map((strX) -> FunctionSig(TyExpr(Syntax.TConVar("Int"), Nil))),
          Map.empty, Map.empty, Nil))
    }

    it("Should function signatures without parentheses") {
      "FUN f : x -> y".as.ast should parse(
        Program(
          List(),
          Map(("f") -> FunctionSig(FunTy(List(TyVar(strX), TyVar(strY))))),
          Map(), Map.empty, Nil))
    }

    it("Should parse higher order function signature") {
      ("FUN map : (x -> y) -> (List x) -> (List y)").as.ast should
        parse(Program(
          List(),
          Map((strMap) -> FunctionSig(FunTy(List(FunTy(List(TyVar(strX), TyVar(strY))),
            TyExpr(Syntax.TConVar(strList), List(TyVar(strX))),
            TyExpr(Syntax.TConVar(strList), List(TyVar(strY))))))),
          Map.empty, Map.empty, Nil))
    }

    it("Should parse other  higher order function signature") {
      ("FUN &= : (DOM a) -> (a -> (DOM b)) -> (DOM b)").as.ast should
        parse(Program(
          List(),
          Map(("&=") -> FunctionSig(
            FunTy(List(
              TyExpr(Syntax.TConVar("DOM"), List(TyVar("a"))),
              FunTy(List(
                TyVar("a"), 
                TyExpr(Syntax.TConVar("DOM"), List(TyVar("b")))
              )),
              TyExpr(Syntax.TConVar("DOM"), List(TyVar("b")))
            ))
          )),
          Map.empty, Map.empty, Nil))
    }

    it("Should parse higher order function signature without unneccessary parens") {
      ("FUN &= : DOM a -> (a -> DOM b) -> DOM b").as.ast should
        parse(Program(
          List(),
          Map(("&=") -> FunctionSig(
            FunTy(List(
              TyExpr(Syntax.TConVar("DOM"), List(TyVar("a"))),
              FunTy(List(
                TyVar("a"), 
                TyExpr(Syntax.TConVar("DOM"), List(TyVar("b")))
              )),
              TyExpr(Syntax.TConVar("DOM"), List(TyVar("b")))
            ))
          )),
          Map.empty, Map.empty, Nil))
    }
  }


  describe(testedImplementationName() + " Test case 11 : Qualified identifiers") {
    it("Should parse qualified operators") {
      ("12.1 Real.+ 14.6").as.expr should parse(App(App(ExVar(Syntax.Var("+","Real")), ConstReal(12.1)),ConstReal(14.6)))
    }

    it("Should parse qualified variables") {
      ("Real.pi").as.expr should parse(ExVar(Syntax.Var("pi","Real")))
    }

    it("Should parse qualified constructors") {
      ("Real.Number").as.expr should parse(ExCon(Syntax.ConVar("Number","Real")))
    }

    it("Should parse qualified identifiers with whitespace around the dot") {
      ("Real. pi Real . + Real .pi").as.expr should parse(
        App(App(
          ExVar(Syntax.Var("+","Real")),
          ExVar(Syntax.Var("pi","Real"))),
          ExVar(Syntax.Var("pi","Real"))))
    }

    it("Should parse simple constant signature with qualified type") {
      "FUN x : Real.Number".as.ast should
        parse(Program(
          List(),
          Map((strX) -> FunctionSig(TyExpr(Syntax.TConVar("Number","Real"), Nil))),
          Map.empty, Map.empty, Nil))
    }

    val str = """
    |CASE y 
    |  OF P.Nil THEN P.Nil
    |  OF P.Cons x xs THEN P.Cons (f x) (map f xs)    
    |""".stripMargin.trim

    def appCons(ft: App, rt: App) = App(App(ExCon(Syntax.ConVar(consLex, "P")), ft), rt)

    it("Should parse qualified list pattern matching") {
      str.as.expr should parse(
        Case(exVar("y"),
          List(Alternative(PatternExpr(Syntax.ConVar(nilLex, "P"), Nil), ExCon(Syntax.ConVar(nilLex, "P"))),
            Alternative(PatternExpr(Syntax.ConVar(consLex, "P"), List(PatternVar("x"), PatternVar("xs"))),
              appCons(App(exVar("f"), exVar("x")), App(App(exVar("map"), exVar("f")), exVar("xs")))))))
    }
  }

  describe(testedImplementationName() + " Test case 12 : Import statements") {
    it("should parse import statement") {
      ("IMPORT \"my/test/module\" AS Mtm").as.ast should 
        parse(Program(
          List(QualifiedImport("my/test/module","Mtm")),
          Map.empty,
          Map.empty, Map.empty,
          Nil))
    }
  }
  
  describe(testedImplementationName() + " Test case 13 : Import Extern statements") {
    it("should parse extern import statement") {
      ("IMPORT EXTERN \"my/test/module\"").as.ast should 
        parse(Program(
          List(ExternImport("my/test/module")),
          Map.empty,
          Map.empty, Map.empty,
          Nil))
    }
  }

  //expr01 := (g x)

  val strF = "f"
  val strG = "g"
  val strX = "x"
  val strY = "y"
  val strZ = "z"
  val strXs = "xs"
  val strMap = "map"
  val strProduct = "Product"
  val strList = "List"

  val varF = exVar(strF)
  val varG = exVar(strG)
  val varX = exVar(strX)
  val varY = exVar(strY)
  val varZ = exVar(strZ)
  val varXs = exVar(strXs)
  val varMap = exVar(strMap)
  val varAdd = exVar(addLex)
  val varMul = exVar(mulLex)
  val patF = PatternVar(strF)
  val patG = PatternVar(strG)
  val patX = PatternVar(strX)
  val patY = PatternVar(strY)
  val patZ = PatternVar(strZ)
  val patXs = PatternVar(strXs)
}

