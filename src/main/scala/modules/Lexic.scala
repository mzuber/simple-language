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

package de.tuberlin.uebb.sl2.modules

/**
 * Lexic of SL
 */
trait Lexic {
  val mainLex = "main"

  /* The predefined functions */
  val addLex = "+"
  val subLex = "-"
  val mulLex = "*"
  val divLex = "/"
  val ltLex = "<"
  val leLex = "<="
  val eqLex = "=="
  val neLex = "/="
  val geLex = ">="
  val gtLex = ">"
  val yieldLex = "yield"
  val bindLex = "&="
  val bindNRLex = "&"
  val stolLex = "stol"
  val ltosLex = "ltos"

  val arithOps = List(addLex, subLex, mulLex, divLex)
  val cmpOps = List(ltLex, leLex, eqLex, neLex, geLex, gtLex)
  val monadicOps = List(yieldLex, bindLex, bindNRLex)
  val stringOps = List(stolLex, ltosLex)
  val predefinedOps = monadicOps ++ arithOps ++ cmpOps ++ stringOps

  val ordLex = "ord"
  val chrLex = "chr"

  val predefinedFuns = (predefinedOps ++ List(ordLex, chrLex)).map(Syntax.Var(_))

  /* Keywords and symbols */
  val defLex = "DEF"
  val funLex = "FUN"
  val dataLex = "DATA"
  val ifLex = "IF"
  val thenLex = "THEN"
  val elseLex = "ELSE"
  val caseLex = "CASE"
  val ofLex = "OF"
  val letLex = "LET"
  val inLex = "IN"
  val importLex = "IMPORT"
  val externLex = "EXTERN"
  val publicLex = "PUBLIC"
  val asLex = "AS"

  val keywords = List(defLex, dataLex, ifLex, thenLex, elseLex,
    caseLex, ofLex, letLex, inLex, funLex, importLex, externLex, publicLex, asLex)

  val dotLex = "."
  val funEqLex = "="
  val dataSepLex = "|"
  val lambdaLex = "\\"
  val arrowLex = "->"
  val typeLex = ":"
  val jsOpenLex = "{|"
  val jsCloseLex = "|}"

  /* Symbols for comments */
  val commentStart = "{-"
  val commentEnd = "-}"
  val commentLine = "--"

  /* Predefined types */
  val intLex = "Int"
  val charLex = "Char"
  val strLex = "String"
  val realLex = "Real"
  val domLex = "DOM"
  val voidLex = "Void"

  val predefinedTypes = List(intLex, charLex, strLex, domLex, voidLex).map(Syntax.TConVar(_))

  /* Predefined constructors (list and bool) */
  val listLex = "List"
  val consLex = "Cons"
  val nilLex = "Nil"
  val trueLex = "True"
  val falseLex = "False"

  val predefCons = List(listLex, consLex, nilLex, trueLex, falseLex).map(Syntax.ConVar(_))
}
