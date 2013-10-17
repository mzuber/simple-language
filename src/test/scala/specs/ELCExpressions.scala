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

  implicit class DefinitionBuilder(lhs: VarName) {
    def :=(rhs: ELC) = EDefinition(Syntax.Var(lhs), None, rhs)
  }

  implicit class ApplicationBuilder(fun: ELC) {
    def :@(expr: ELC) = EApp(fun, expr)
  }

  def pVar(ide: VarName) = EPatternVar(ide)

  def pApp(con: ConVarName, pats: EPattern*) = EPatternApp(Syntax.ConVar(con), pats.toList)
  def pApp(con: ConVar, pats: EPattern*) = EPatternApp(con, pats.toList)

  def eVar(name: String): ELC = EVar(Syntax.Var(name), EmptyAttribute);
  def eCon(name: String): ELC = ECon(Syntax.ConVar(name), EmptyAttribute);
  
  def eChoice(choices: ELC*) = EChoice(choices.toList)

  def eCase(expr: ELC, alts: EAlternative*) = ECase(expr, alts.toList)

  def tv(ide: TypeVarName) = TypeVariable(ide)


  /*
   * id = λ x. x
   */
  val idDef = "id" := (pVar("x") :=> eVar("x"))


  /*
   * length = CHOICE
   *            λ Nil. 0
   *            λ (Cons x xs). 1 + (length xs)
   */
  val lengthDef = "length" := {
    eChoice( pApp("Nil") :=> EInt(0),
	     pApp("Cons", pVar("x"), pVar("xs")) :=> ((eVar("+") :@ EInt(1)) :@ (eVar("length") :@ eVar("xs"))) )
  }


  /*
   * sum = CHOICE
   *         λ Nil. 0
   *         λ (Cons x xs). x + (length xs)
   */
  val sumDef = "sum" := {
    eChoice( pApp("Nil") :=> EInt(0),
	     pApp("Cons", pVar("x"), pVar("xs")) :=> ((eVar("+") :@ eVar("x")) :@ (eVar("sum") :@ eVar("xs"))) )
  }


  /*
   * append = λ xs. λ ys. CASE xs
   *                        OF Nil THEN ys
   *                        OF Cons x xs1 THEN Cons x (append xs1 ys)
   */
  val appendDef = "append" := {
    pVar("xs") :=> {
      pVar("ys") :=> {
	eCase(eVar("xs"),
	      EAlternative(pApp("Nil"), eVar("ys")),
              EAlternative(pApp("Cons", pVar("x"), pVar("xs1")),
			   (eCon("Cons") :@ eVar("x")) :@ ((eVar("append") :@ eVar("xs1")) :@ eVar("ys"))))
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
	eCase(eVar("l"),
	      EAlternative(pApp("Nil"), eCon("Nil")),
	      EAlternative(pApp("Cons", pVar("x"), pVar("xs")),
			   (eCon("Cons") :@ (eVar("f") :@ eVar("x"))) :@ ((eVar("map") :@ eVar("f")) :@ eVar("xs"))))
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
               pApp("Nil") :=> eCon("Nil")
             },
	     pVar("f") :=> {
	       pApp("Cons", pVar("x"), pVar("xs")) :=> ((eCon("Cons") :@ (eVar("f") :@ eVar("x"))) :@ ((eVar("map") :@ eVar("f")) :@ eVar("xs")))
	   } )
  }


  /*
   * even = λ n. CASE n == 0
   *               OF True  THEN True
   *               OF False THEN odd (n - 1)
   */
  val evenDef = "even" := {
    pVar("n") :=> eCase((eVar("==") :@ eVar("n")) :@ EInt(0),
		        EAlternative(pApp("True"), eCon("True")),
		        EAlternative(pApp("False"), eVar("odd") :@ ((eVar("-") :@ eVar("n")) :@ EInt(1))))
  }


  /*
   * odd = λ n. CASE n == 1
   *              OF True THEN True
   *              OF False THEN even (n - 1)
   */
  val oddDef = "odd" := {
    pVar("n") :=> eCase((eVar("==") :@ eVar("n")) :@ EInt(1),
		        EAlternative(pApp("True"), eCon("True")),
		        EAlternative(pApp("False"), eVar("even") :@ ((eVar("-") :@ eVar("n")) :@ EInt(1))))
  }
}
