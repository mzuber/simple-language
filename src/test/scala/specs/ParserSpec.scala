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

package de.tuberlin.uebb.sl2.tests.specs

import scala.language.implicitConversions

import org.scalatest.matchers._
import org.scalatest.{ FunSpec, Inside }

import de.tuberlin.uebb.sl2.modules._

/**
  *
  */
trait ParserSpec extends FunSpec with Inside with ShouldMatchers {

  this: Parser with Syntax with Lexic with Errors =>

  case class ParseCommand[T](s: String, cmd: String => Either[Error, T])

  implicit class ParsingInput(s: String) {
    def as() = Parsing(s)
  }

  def functionDef2Modul(n: Var, d: List[FunctionDef]): AST = Program( Map.empty, Map(n -> d), Nil)
  def dataDef2Modul(d: DataDef): AST = Program( Map.empty, Map.empty, List(d))
  def functionDefs2Modul(d: Map[Var, List[FunctionDef]]): AST = Program( Map.empty, d, Nil)

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


  describe(testedImplementationName() + ": Basic tests") {
    it("Should parse Integer literals") {
      "42".as.expr should parse(ConstInt(42))
    }

    it("Should parse negative Integer literals") {
      "-42".as.expr should parse(ConstInt(-42))
    }
    it("Should parse negative Integer literals with whitespace") {
      "- 42".as.expr should parse(ConstInt(-42))
    }

    it("Should parse Character literals") {
      "'c'".as.expr should parse(ConstChar('c'))
    }

    it("Should parse string-literals correctly") {
      """f "x" x "x"""".as.expr should parse (App(App(App(ExVar("f"), ConstString("x")), ExVar("x")), ConstString("x")))
    }

    it("Should parse variables") {
      "x".as.expr should parse(ExVar("x"))
    }
    it("Should parse variables with parentheses") {
      "(x)".as.expr should parse(ExVar("x"))
    }

    it("Should parse multi-letter variables") {
      "foo".as.expr should parse(ExVar("foo"))
    }

    it("Should parse multi-letter constructors") {
      "Foo".as.expr should parse(ExCon("Foo"))
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
      "x  {-comment-}".as.expr should parse(ExVar("x"))
    }

    it("Should parse constructors followed by a comment") {
      "Foo {-comment-}".as.expr should parse(ExCon("Foo"))
    }

    it("Should parse string-literals followed by a comment") {
      """"TUB" {-comment-}""".as.expr should parse(ConstString("TUB"))
    }

    it("Should parse simple function definition") {
      "DEF f x = x".as.ast should parse(functionDef2Modul("f", List(FunctionDef(List(PatternVar("x")), ExVar("x")))))
    }

    it("Should parse binary opertor definition") {
      "DEF x ++ y = x".as.ast should parse(functionDef2Modul("++", List(FunctionDef(List(PatternVar("x"),PatternVar("y")), ExVar("x")))))
    }

    it("Should parse constructor applications non-greedy") {
      inside(parseExpr("""\ Cons Nil Nil . True""").right.get) {
        case Lambda(l, _, _) => l.size should be (3)
      }
    }

    it("Should parse function with two definitions") {
      """DEF f Nil = 0 
         DEF f (Cons a b) = 1""".as.ast should parse((functionDef2Modul("f", List(
             FunctionDef(List(PatternExpr("Cons", List(PatternVar("a"), PatternVar("b")))), ConstInt(1)), 
             FunctionDef(List(PatternExpr("Nil", Nil)), ConstInt(0))))))
    }
  }

  describe(testedImplementationName() + ": Function application") {
    it("Should parse a function application") {
      "g x y".as.expr should parse(App(App(ExVar("g"), ExVar("x")), ExVar("y")))
    }
    
    it("Should parse application cut off by a comment") {
      "f {-42-} x".as.expr should parse(App(ExVar("f"), ExVar("x")))
    }

    it("Should parse custom binary operator application") {
      "x +++ y".as.expr should parse(App(App(ExVar("+++"), ExVar("x")), ExVar("y")))
    }

    it("Should invert application order in case of operators") {
      "x - y".as.expr should parse(App(App(ExVar(subLex), ExVar("x")), ExVar("y")))
    }

    
    it("Should parse a function application mixed with an operator") {
      "g x y + z".as.expr should parse(App(App(ExVar(addLex), App(App(ExVar("g"), ExVar("x")), ExVar("y"))), ExVar("z")))
    }

    it("Should parse a minus before arithmetic expression") {
      "-(x + y)".as.expr should parse(App(App(ExVar(mulLex), ConstInt(-1)), App(App(ExVar(addLex), ExVar("x")), ExVar("y"))))
    }
    
    it("Should parse binary minus in arithmetic expression") {
      "x - 1".as.expr should parse(App(App(ExVar(subLex), ExVar("x")), ConstInt(1)))
    }
    
    
    it("Should parse string concatenation") {
      "x +s y".as.expr should parse(App(App(ExVar(strAdd), ExVar("x")), ExVar("y")))
    }
  }

  describe(testedImplementationName() + ": Operator precedence") {

    it("Should bind rhs multiplication over lhs comparison") {
      "x == y * z".as.expr should parse(App(App(ExVar(eqLex), ExVar("x")), App(App(ExVar(mulLex), ExVar("y")), ExVar("z"))))
    }

    it("Should bind rhs multiplication over lhs addition") {
      "x + y * z".as.expr should parse(App(App(ExVar(addLex), ExVar("x")), App(App(ExVar(mulLex), ExVar("y")), ExVar("z"))))
    }

    it("Should bind rhs division over lhs subtraction") {
      "x - y / z".as.expr should parse(App(App(ExVar(subLex), ExVar("x")), App(App(ExVar(divLex), ExVar("y")), ExVar("z"))))
    }

    it("Should bind lhs multiplication over rhs addition") {
      "x * y + z".as.expr should parse(App(App(ExVar(addLex), App(App(ExVar(mulLex), ExVar("x")), ExVar("y"))), ExVar("z")))
    }

    it("Should bind lhs division over lhs subtraction") {
      "x / y - z".as.expr should parse(App(App(ExVar(subLex), App(App(ExVar(divLex), ExVar("x")), ExVar("y"))), ExVar("z")))
    }

    it("Should bind custom ops lower as addition") {
      "1 + 3 +++ 2".as.expr should parse(App( App(ExVar("+++"), App(App(ExVar("+"), ConstInt(1)), ConstInt(3)) ), ConstInt(2)))
    }

    it("Should bind custom ops lower as multiplication") {
      "1 * 3 +++ 2".as.expr should parse(App( App(ExVar("+++"), App(App(ExVar("*"), ConstInt(1)), ConstInt(3)) ), ConstInt(2)))
    }

    it("Should bind custom ops lower as comparison") {
      "1 == 3 +++ 2".as.expr should parse(App( App(ExVar("+++"), App(App(ExVar("=="), ConstInt(1)), ConstInt(3)) ), ConstInt(2)))
    }
  }

  describe(testedImplementationName() + ": IF / THEN / ELSE") {

    it("Should parse a simple IF-THEN-ELSE") {
      "IF x THEN 1 ELSE 10".as.expr should parse(Conditional(ExVar("x"), ConstInt(1), ConstInt(10)))
    }

    it("Should parse a nested IF-THEN-ELSE") {
      "IF IF 1 THEN x ELSE y THEN 1 ELSE 10".as.expr should parse(Conditional(Conditional(ConstInt(1), ExVar("x"), ExVar("y")), ConstInt(1), ConstInt(10)))
    }
  }

  describe(testedImplementationName() + ": Pattern matching") {
    it("Should parse patter matching in function definition") {
      ("DEF map g Nil = Nil\n" + "DEF map g (Cons x y) = Cons (g x) (map g y)").as.ast should parse(functionDefs2Modul(
        Map(
          "map" -> List(
            FunctionDef(
              List(PatternVar("g"), PatternExpr(consLex, List(PatternVar("x"), PatternVar("y")))),
              App(App(ExCon(consLex), App(ExVar("g"), ExVar("x"))), App(App(ExVar("map"), ExVar("g")), ExVar("y")))),
            FunctionDef(List(PatternVar("g"), PatternExpr(nilLex, Nil)), ExCon(nilLex))))))
    }
    
   it("Should parse patter matching in function definition with variable") {
      ("DEF map Nil g = Nil\n").as.ast should parse(functionDefs2Modul(
        Map(
          "map" -> List(
            FunctionDef(List(PatternExpr(nilLex, Nil), PatternVar("g")), ExCon(nilLex))))))
    }
    
  }

  describe(testedImplementationName() + ": LET-expressions") {

    it("Should parse a let-expression with one variable") {
      "LET x = 1 IN x".as.expr should parse(Let(List(LetDef("x", ConstInt(1))), ExVar("x")))
    }

    it("Should parse a let-expression with multiple variables") {
      "LET x = 10 y = 20 IN x".as.expr should parse(Let(List(LetDef("x", ConstInt(10)), LetDef("y", ConstInt(20))), ExVar("x")))
    }

    it("Should parse a let-expression with operater beginning with =") {
      "LET x = 10 == 3 IN x".as.expr should parse(Let(List(LetDef("x", App(App(ExVar("=="), ConstInt(10)), ConstInt(3)))), ExVar("x")))
    }
  }

  describe(testedImplementationName() + ": Lambda-expressions") {

    it("Should parse the identity function") {
      ("\\ x . x").as.expr should parse(Lambda(List(PatternVar("x")), ExVar("x")))
    }

    it("Should parse " + lambdaLex + " x y . y") {
      ("\\ x y . y").as.expr should parse(Lambda(List(PatternVar("x"), PatternVar("y")), ExVar("y")))
    }

    it("Should parse " + lambdaLex + " Nil . 1") {
      ("\\ Nil . 1").as.expr should parse(Lambda(List(PatternExpr(nilLex, Nil)), ConstInt(1)))
    }

  }

  describe(testedImplementationName() + ": Case-expressions") {
    val str = """
    |CASE y 
    |  OF Nil THEN Nil
    |  OF Cons x xs THEN Cons (f x) (map f xs)    
    |""".stripMargin.trim

    def appCons(ft: App, rt: App) = App(App(ExCon(consLex), ft), rt)

    it("Should parse list pattern matching") {
      str.as.expr should parse(
        Case(ExVar("y"),
          List(Alternative(PatternExpr(nilLex, Nil), ExCon(nilLex)),
            Alternative(PatternExpr(consLex, List(PatternVar("x"), PatternVar("xs"))),
              appCons(App(ExVar("f"), ExVar("x")), App(App(ExVar("map"), ExVar("f")), ExVar("xs")))))))
    }

    val much = 10000 // increase for combinator blowup
    it("Should parse quite large pattern matching expressions") {
      inside(parseExpr("CASE y " + ("OF Nil THEN Nil " * much))) {
        case Right(Case(ExVar(y, _), alternatives, _)) => {
          y should be("y")
          alternatives should have length (much)
        }
      }
    }
  }

  describe(testedImplementationName() + ": Data type definitions") {
    it("Should parse simple data type definition") {
      "DATA Product x y = Product x y".as.ast should parse(dataDef2Modul(DataDef("Product",
        List("x", "y"),
        List(ConstructorDef("Product", List(TyVar("x"), TyVar("y")))))))
    }

    it("Should parse algebraic data type definition") {
      "DATA List x = Cons x (List x) | Nil".as.ast should parse(dataDef2Modul(DataDef("List", List("x"),
        List(
          ConstructorDef(consLex, List(
            TyVar("x"),
            TyExpr("List", List(
              TyVar("x"))))),
          ConstructorDef(nilLex, Nil)))))
    }
    
    it("Should parse tree data type definition without argument") {
      "DATA Tree =  Node Tree Tree | Leaf Int ".as.ast should parse(dataDef2Modul(DataDef("Tree", Nil,
        List(
          ConstructorDef("Node", List(
            TyExpr("Tree",Nil),TyExpr("Tree",Nil))),
          ConstructorDef("Leaf", List(TyExpr("Int",Nil)))))))
    }
    
    it("Should parse tree data type definition with argument") {
      "DATA Tree a =  Node (Tree a) (Tree a) | Leaf a ".as.ast should parse(dataDef2Modul(DataDef("Tree", List("a"),
        List(
          ConstructorDef("Node", List(
            TyExpr("Tree",List(TyVar("a"))),TyExpr("Tree",List(TyVar("a"))))),
          ConstructorDef("Leaf",List(TyVar("a")))))))
    }
    
    it("Should parse function types inside DATA defs") {
      "DATA Fun = IntFun (Int -> Int)".as.ast should parse(dataDef2Modul(
        DataDef("Fun", Nil, 
                ConstructorDef("IntFun", FunTy(TyExpr("Int",Nil)::TyExpr("Int",Nil)::Nil)::Nil)::Nil)))
    }
    
  }

  describe(testedImplementationName() + ": Function signatures") {
    it("Should parse simple function signature") {
      "FUN x : (Int) -> (Int)".as.ast should
        parse(Program(
          Map("x" -> FunctionSig(FunTy(List(TyExpr("Int", Nil), TyExpr("Int", Nil))))),
          Map.empty, Nil))
    }

    it("Should parse function signatures followed by DEFs") {
      val expectedDefs = FunctionDef(List(PatternVar("x")), ExVar("x"))::Nil
      "FUN int2Str : Int -> String DEF id x = x".as.ast should
        parse(Program(
          Map("int2Str" -> FunctionSig(FunTy(List(TyExpr("Int", Nil), TyExpr("String", Nil))))),
          Map("id" -> expectedDefs),Nil))
    }

    it("Should parse function signatures after DATA defs") {

      """DATA List x = Cons x (List x) | Nil FUN int2Str : Int -> String""".as.ast should 
      parse( Program(Map("int2Str" -> FunctionSig(FunTy(List(TyExpr("Int", Nil), TyExpr("String", Nil))))),
                    Map(), 
                    DataDef("List", List("x"),
                            List(
                              ConstructorDef(consLex, List(
                                TyVar("x"),
                                TyExpr("List", List(
                                  TyVar("x"))))),
                              ConstructorDef(nilLex, Nil)))::Nil)
            )
    }
            
    it("Should parse simple constant signature") {
      "FUN x : Int".as.ast should
        parse(Program(
          Map("x" -> FunctionSig(TyExpr("Int", Nil))),
          Map.empty, Nil))
    }
    
    it("Should function signatures without parentheses") {
      "FUN f : x -> y".as.ast should parse(
        Program(
          Map("f" -> FunctionSig(FunTy(List(TyVar("x"), TyVar("y"))))),
          Map(), Nil))
    }
    
    it("Should parse higher order function signature") {
      ("FUN map : (x -> y) -> (List x) -> (List y)").as.ast should
        parse(Program(
          Map("map" -> FunctionSig(FunTy(List(FunTy(List(TyVar("x"), TyVar("y"))),
            TyExpr("List", List(TyVar("x"))),
            TyExpr("List", List(TyVar("y"))))))),
          Map.empty, Nil))
    }
  }
}
