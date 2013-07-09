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

import de.tuberlin.uebb.sl2.modules._

/**
  * ELC expressions, definitions and patterns for testing purposes.
  */
trait ELCExpressions {

  this: Syntax with EnrichedLambdaCalculus with Type =>

  /*
   * Convenience functions for constructing ELC expressions and definitions manually. 
   */
  implicit class LambdaAbstractionBuilder(pat: EPattern) {
    def :=>(body: ELC) = ELam(pat, body)
  }

  implicit class DefinitionBuilder(lhs: Var) {
    def :=(rhs: ELC) = EDefinition(lhs, None, rhs)
  }

  implicit class ApplicationBuilder(fun: ELC) {
    def :@(expr: ELC) = EApp(fun, expr)
  }

  def pVar(ide: Var) = EPatternVar(ide)

  def pApp(con: ConVar, pats: EPattern*) = EPatternApp(con, pats.toList)

  def eChoice(choices: ELC*) = EChoice(choices.toList)

  def eCase(expr: ELC, alts: EAlternative*) = ECase(expr, alts.toList)

  def tv(ide: TypeVar) = TypeVariable(ide)


  /*
   * id = λ x. x
   */
  val idDef = "id" := (pVar("x") :=> EVar("x"))


  /*
   * length = CHOICE
   *            λ Nil. 0
   *            λ (Cons x xs). 1 + (length xs)
   */
  val lengthDef = "length" := {
    eChoice( pApp("Nil") :=> EInt(0),
	     pApp("Cons", pVar("x"), pVar("xs")) :=> ((EVar("+") :@ EInt(1)) :@ (EVar("length") :@ EVar("xs"))) )
  }


  /*
   * sum = CHOICE
   *         λ Nil. 0
   *         λ (Cons x xs). x + (length xs)
   */
  val sumDef = "sum" := {
    eChoice( pApp("Nil") :=> EInt(0),
	     pApp("Cons", pVar("x"), pVar("xs")) :=> ((EVar("+") :@ EVar("x")) :@ (EVar("sum") :@ EVar("xs"))) )
  }


  /*
   * append = λ xs. λ ys. CASE xs
   *                        OF Nil THEN ys
   *                        OF Cons x xs1 THEN Cons x (append xs1 ys)
   */
  val appendDef = "append" := {
    pVar("xs") :=> {
      pVar("ys") :=> {
	eCase(EVar("xs"),
	      EAlternative(pApp("Nil"), EVar("ys")),
              EAlternative(pApp("Cons", pVar("x"), pVar("xs1")),
			   (ECon("Cons") :@ EVar("x")) :@ ((EVar("append") :@ EVar("xs1")) :@ EVar("ys"))))
      }
    }
  }

  
  /*
   * map = λ f. λ l. CASE l
   *                   OF Nil THEN Nil
   *                   OF Cons x xs THEN Cons (f x) (map f xs)
   */
  val mapDef = "map" := {
    pVar("f") :=> {
      pVar("l") :=> {
	eCase(EVar("l"),
	      EAlternative(pApp("Nil"), EVar("Nil")),
	      EAlternative(pApp("Cons", pVar("x"), pVar("xs")),
			   (ECon("Cons") :@ (EVar("f") :@ EVar("x"))) :@ ((EVar("map") :@ EVar("f")) :@ EVar("xs"))))
      }
    }
  }


  /*
   * map = CHOICE
   *         λ f. λ Nil. Nil
   *         λ f. λ Cons x xs. Cons (f x) (map f xs)
   */
  val mapChoiceDef = "map" := {
    eChoice( pVar("f") :=> {
               pApp("Nil") :=> EVar("Nil")
             },
	     pVar("f") :=> {
	       pApp("Cons", pVar("x"), pVar("xs")) :=> ((ECon("Cons") :@ (EVar("f") :@ EVar("x"))) :@ ((EVar("map") :@ EVar("f")) :@ EVar("xs")))
	   } )
  }


  /*
   * even = λ n. CASE n == 0
   *               OF True  THEN True
   *               OF False THEN odd (n - 1)
   */
  val evenDef = "even" := {
    pVar("n") :=> eCase((EVar("==") :@ EVar("n")) :@ EInt(0),
		        EAlternative(pApp("True"), EVar("True")),
		        EAlternative(pApp("False"), EVar("odd") :@ ((EVar("-") :@ EVar("n")) :@ EInt(1))))
  }


  /*
   * odd = λ n. CASE n == 1
   *              OF True THEN True
   *              OF False THEN even (n - 1)
   */
  val oddDef = "odd" := {
    pVar("n") :=> eCase((EVar("==") :@ EVar("n")) :@ EInt(1),
		        EAlternative(pApp("True"), EVar("True")),
		        EAlternative(pApp("False"), EVar("even") :@ ((EVar("-") :@ EVar("n")) :@ EInt(1))))
  }
}
