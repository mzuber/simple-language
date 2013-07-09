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

import org.scalatest.matchers._
import org.scalatest.FunSpec
import de.tuberlin.uebb.sl2.modules._

trait TypeCheckerSpec extends FunSpec with ShouldMatchers {

  this: TypeChecker with Context with Type with NameSupply with EnrichedLambdaCalculus with ELCExpressions with Errors =>

  def testedImplementationName(): String

  /*
   * Prelude and built-in functions
   */
  val int  = BaseType(Integer)
  val char = BaseType(Character)
  val bool = TypeConstructor("Bool", Nil)
  val α    = TypeVariable("alpha")
  val β    = TypeVariable("beta")
  val list = (t: Type) => TypeConstructor("List", List(t))

  val initialContext: Context = {
    Map( "True"  -> bool,
	 "False" -> bool,
	 "Nil"   -> TypeScheme(List(α), list(α)),
	 "Cons"  -> TypeScheme(List(α), α --> (list(α) --> list(α))),
	 "Pair"  -> TypeScheme(List(α, β), α --> (β --> TypeConstructor("Pair", List(α, β))))
       )
  }

  /*
   * Custom Matcher handling the Either values returned by the 'checkType' function.
   */
  case class TypeCheckMatcher(expected: Type) extends Matcher[ELC] {

    def apply(expr: ELC) = {
      val result = checkTypes(initialContext <++> predefsContext, expr)

      val failureMessageSuffix = (for (resType <- result.right) yield "'" + expr + "' resulted in " + resType + " which did not equal " + expected).fold("failed: %s".format(_), _.toString)

      val negatedFailureMessageSuffix = "'" + expr + "' yielded " + expected

      val equals = (for (resType <- result.right) yield resType == expected).right.toOption.getOrElse(false)

      MatchResult(equals,
		  "Type checking " + failureMessageSuffix,
		  "Type checking " + negatedFailureMessageSuffix,
		  "Type checking " + failureMessageSuffix,
		  "Type checking " + negatedFailureMessageSuffix)
    }
  }


  def haveType(ty: Type) = TypeCheckMatcher(ty)


  describe(testedImplementationName() + ": Built-in values") {
    it("Should type check Integer values") {
      EInt(42) should haveType(int)
    }

    it("Should type check Character values") {
      EChar('c') should haveType(char)
    }

    it("Should type check String values") {
      EStr("Test") should haveType(BaseType(String))
    }
  }

  describe(testedImplementationName() + ": Lambda abstraction") {
    it("Should type check lambda abstractions over variable patterns") {
      reset() // Reset name supply
      (pVar("x") :=> EVar("x")) should haveType(tv("$0") --> tv("$0"))
    }
    
    it("Should type check lambda abstractions over constructor patterns") {
      reset() // Reset name supply
      val expr = pApp("Cons", pVar("x"), pVar("xs")) :=> EVar("x")
      expr should haveType(list(tv("$1")) --> tv("$1"))
    }
  }

  describe(testedImplementationName() + ": Application") {
    it("Should type check application of built-in function 'chr'") {
      (EVar("chr") :@ EInt(42)) should haveType(char)
    }

    it("Should type check application of built-in function 'ord'") {
      (EVar("ord") :@ EChar('c')) should haveType(int)
    }

    it("Should type check application of built-in function '+'") {
      ((EVar("+") :@ EInt(1)) :@ EInt(1)) should haveType(int)
    }

    it("Should type check application of built-in function '=='") {
      ((EVar("==") :@ EInt(1)) :@ EInt(1)) should haveType(bool)
    }

    it("Should type check function application '(λ x.x) (λ x.x)'") {
      reset() // Reset name supply
      val id = pVar("x") :=> EVar("x")
      (id :@ id) should haveType(tv("$1") --> tv("$1"))
    }
  }

  describe(testedImplementationName() + ": Let-bindings") {
    it("Should type check monomorphic let-binding") {
      ELet(idDef, EVar("id") :@ EInt(5)) should haveType(int)
    }

    it("Should type check polymorphic let-binding") {
      val body = (ECon("Pair") :@ (EVar("id") :@ EInt(5))) :@ (EVar("id") :@ EVar("True"))
      ELet(idDef, body) should haveType(TypeConstructor("Pair", List(int, bool)))
    }
  }

  describe(testedImplementationName() + ": Recursive let-bindings") {
    it("Should type check CASE-based 'map' definition") {
      reset() // Reset name supply
      val mapType = (tv("$14") --> tv("$15")) --> (list(tv("$14")) --> list(tv("$15")))
      ELetRec(List(mapDef), EVar("map")) should haveType(mapType)
    }

    it("Should type check CHOICE-based 'map' definition") {
      reset() // Reset name supply
      val mapType = (tv("$14") --> tv("$15")) --> (list(tv("$14")) --> list(tv("$15")))
      ELetRec(List(mapChoiceDef), EVar("map")) should haveType(mapType)
    }

    it("Should type check 'append' definition") {
      reset() // Reset name supply
      val appendType = list(tv("$12")) --> (list(tv("$12")) --> list(tv("$12")))
      ELetRec(List(appendDef), EVar("append")) should haveType(appendType)
    }

    it("Should type check 'length' definition") {
      reset() // Reset name supply
      ELetRec(List(lengthDef), EVar("length")) should haveType(list(tv("$8")) --> int)
    }

    it("Should type check 'sum' definition") {
      ELetRec(List(sumDef), EVar("sum")) should haveType(list(int) --> int)
    }
  }

  describe(testedImplementationName() + ": Mutually recursive let-bindings") {
    it("Should type check {'even', 'odd'} definitions") {
      ELetRec(List(evenDef, oddDef), EVar("even") :@ EInt(42)) should haveType(bool)
    }
  }

  describe(testedImplementationName() + ": Multiple definitions in let-bindings") {
    it("Should type check {'map', 'append', 'even', 'length', 'odd', 'sum'} definitions") {
      ELetRec(List(mapDef, appendDef, evenDef, lengthDef, oddDef, sumDef), EVar("odd") :@ EInt(42)) should haveType(bool)
    }
  }

}
