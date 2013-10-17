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
import org.scalatest.FunSpec
import de.tuberlin.uebb.sl2.modules._

trait LetRecSplitterSpec extends FunSpec with ShouldMatchers {

  this: LetRecSplitter with Lexic with EnrichedLambdaCalculus with ELCExpressions with Errors =>

  def testedImplementationName: String


  /*
   * Custom Matcher handling the Either values returned by the 'checkType' function.
   */
  case class LetRecSplittingMatcher(expected: ELC) extends Matcher[ELC] {

    val predefs: Set[Syntax.VarFirstClass] =
      predefinedFuns.toSet ++
      Set("True", "False", "List", "Cons", "Nil").map(Syntax.ConVar(_))

    def apply(expr: ELC) = {
      val result = splitLetRecs(predefs, expr)

      val failureMessageSuffix = (for (splittedExpr <- result.right) yield "\n'" + expr + "' resulted in\n" + splittedExpr + "\nwhich did not equal\n" + expected).fold("failed: %s".format(_), _.toString)

      val negatedFailureMessageSuffix = "'" + expr + "' yielded " + expected

      val equals = (for (splittedExpr <- result.right) yield splittedExpr == expected).right.toOption.getOrElse(false)

      MatchResult(equals,
		  "Splitting " + failureMessageSuffix,
		  "Splitting " + negatedFailureMessageSuffix,
		  "Splitting " + failureMessageSuffix,
		  "Splitting " + negatedFailureMessageSuffix)
    }
  }


  def splitTo(expr: ELC) = LetRecSplittingMatcher(expr)


  describe(testedImplementationName + ": Mutually recursive definitions") {
    it("{Even, Odd} should not be splitted") {
      val evenOdd = ELetRec(List(evenDef, oddDef), EInt(0))
      evenOdd should splitTo(evenOdd)
    }

    it("{Id, Even, Odd} should be splitted into {Even, Odd}, {Id}") {
      ELetRec(List(idDef, evenDef, oddDef), EInt(0)) should splitTo(ELet(idDef, ELetRec(List(evenDef, oddDef), EInt(0))))
    }
  }

  describe(testedImplementationName + ": Linear chain of non-recursive definitions") {
    it("{a,b,c,d,e} should be splitted into {a}, {b}, {c}, {d}, {e}") {

      /*
       * let a = ...
       *     b = a
       *     c = b
       *     d = c
       *     e = d
       * in ...
       */
      val letrec = ELetRec(List( "a" := EInt(23) ,
			         "b" := eVar("a") ,
			         "c" := eVar("b") ,
			         "d" := eVar("c") ,
			         "e" := eVar("d") ),
			   EInt(0))

      /*
       * let    a = ...
       * in let b = a
       * in let c = b
       * in let d = c
       * in let e = d
       * in ...
       */
      val splittedLetRec = ELet("a" := EInt(23),
				ELet("b" := eVar("a"),
				     ELet("c" := eVar("b"),
					  ELet("d" := eVar("c"),
					       ELet("e" := eVar("d"),
						    EInt(0))))))

      letrec should splitTo(splittedLetRec)
    }
  }

  describe(testedImplementationName + ": Example from 'The Implementation of Functional Programming Languages', Chapter 6") {
    it("{a,b,c,d,f,g,h} should be splitted into {a}, {f,g,h}, {b}, {d,c}") {
      
      /*
       * let a = ...
       *     b = ... a ...
       *     c = ... h ... b ... d ...
       *     d = ... c ...
       *     f = ... g ... h ... a ...
       *     g = ... f ...
       *     h = ... g ...
       * in ...
       */
      val letrec = ELetRec(List( "a" := EInt(42) ,
			         "b" := eVar("chr") :@ eVar("a") ,
			         "c" := eVar("h") :@ (eVar("b") :@ eVar("d")) ,
			         "d" := pVar("x") :=> (eVar("c") :@ eVar("x")) ,
			         "f" := eVar("g") :@ (eVar("h") :@ eVar("a")) ,
			         "g" := eVar("f") ,
			         "h" := pVar("y") :=> eCase(eVar("y"),
							    EAlternative(pApp("True"), eCon("True")),
							    EAlternative(pApp("False"), eVar("g"))) ),
			   EInt(0))

      /*
       * let       a = ...
       * in letrec f = ... g ... h ... a ...
       *           g = ... f ...
       *           h = ... g ...
       * in let    b = ... a ...
       * in letrec d = ... c ...
       *           c = ... h ... b ... d ...
       * in ...
       */
      val splittedLetRec = ELet( "a" := EInt(42) ,
				 ELetRec(List( "f" := eVar("g") :@ (eVar("h") :@ eVar("a")) ,
					       "g" := eVar("f") ,
					       "h" := pVar("y") :=> eCase(eVar("y"),
									  EAlternative(pApp("True"), eCon("True")),
									  EAlternative(pApp("False"), eVar("g"))) ),
					 ELet( "b" := eVar("chr") :@ eVar("a") ,
					       ELetRec(List( "d" := pVar("x") :=> (eVar("c") :@ eVar("x")) ,
							     "c" := eVar("h") :@ (eVar("b") :@ eVar("d")) ),
						       EInt(0)))))

      letrec should splitTo(splittedLetRec)
    }
  }
}
