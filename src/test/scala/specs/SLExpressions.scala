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
  * SL expressions, definitions and patterns for testing purposes.
  */
trait SLExpressions {

  this: Syntax =>

  /*
   * Data type definitions
   */
  val boolDT = DataDef("Bool", Nil, List(ConstructorDef("True", Nil), ConstructorDef("False", Nil)))

  val intAPairDT = DataDef("IntAPair", List("a"), List(ConstructorDef("IntAPair", List(TyExpr("Int", Nil), TyVar("a")))))

  val pairDT = DataDef("Pair", List("a", "b"), List(ConstructorDef("Pair", List(TyVar("a"), TyVar("b")))))

  val listDT = DataDef("List", List("a"), List(ConstructorDef("Nil", Nil),
					       ConstructorDef("Cons", List(TyVar("a"), TyExpr("List", List(TyVar("a")))))))

  val treeDT = DataDef("Tree", List("a"), List(ConstructorDef("Null", Nil),
					       ConstructorDef("Branch", List(TyVar("a"),
									     TyExpr("List", List(TyExpr("Tree", List(TyVar("a")))))))))

  val bintreeDT = DataDef("BinTree", List("a"), List(ConstructorDef("Tip", Nil),
						     ConstructorDef("Node", List(TyExpr("BinTree", List(TyVar("a"))),
											TyVar("a"),
											TyExpr("BinTree", List(TyVar("a")))))))

  val dupDataConsDT = DataDef("DupDataCons", Nil, List(ConstructorDef("DupCon", Nil),
						     ConstructorDef("DupCon", Nil)))

  val dupTypeParamDT = DataDef("DupTypeParam", List("a", "a"), List(ConstructorDef("C", Nil)))

  val undefConDT = DataDef("UndefConUsed", Nil, List(ConstructorDef("C", List(TyExpr("Undefined", Nil)))))

  val unusedTypeParamsDT = DataDef("UnusedTypeParam", List("a"), List(ConstructorDef("C", Nil)))

  val undefTypeParamsDT = DataDef("UndefTypeParam", Nil, List(ConstructorDef("C", List(TyExpr("BinTree", List(TyVar("a")))))))

  val wrongConsAppDT = DataDef("WrongConsApp", List("a"), List(ConstructorDef("C", List(TyExpr("Bool", List(TyVar("a")))))))

  val allDatatypes = List(boolDT, pairDT, intAPairDT, listDT, treeDT, bintreeDT)


  /*
   * Function definitions
   */
  val idFD = ("id", List(FunctionDef(List(PatternVar("x")), ExVar(("x")))))

  val mapFDnil = FunctionDef(List(PatternVar("f"), PatternExpr("Nil", Nil)), ExCon("Nil"))

  val mapFDnil1 = FunctionDef(List(PatternExpr("Nil", Nil)), ExCon("Nil"))

  val mapFDcons = FunctionDef(List(PatternVar("f"), PatternExpr("Cons", List(PatternVar("x"), PatternVar("xs")))),
			      App(App(ExCon("Cons"), App(ExVar("f"), ExVar("x"))), App(App(ExVar("map"), ExVar("f")), ExVar("xs"))))

  val mapFD = ("map", List(mapFDnil, mapFDcons))

  val mapFD1 = ("map", List(mapFDnil1, mapFDcons))

  val incFD = ("inc", List(FunctionDef(List(PatternVar("x")), App(App(ExVar("+"), ConstInt(1)), ExVar("x")))))

  val decFD = ("dec", List(FunctionDef(List(PatternVar("x")), App(App(ExVar("-"), ConstInt(1)), ExVar("x")))))

  val evenFD = ("even", List(FunctionDef(List(PatternVar("x")), Conditional(App(App(ExVar("=="), ConstInt(0)), ExVar("x")),
									    ExCon("True"),
									    App(ExVar("odd"), App(ExVar("dec"), ExVar("x")))))))

  val oddFD = ("odd", List(FunctionDef(List(PatternVar("x")), Conditional(App(App(ExVar("=="), ConstInt(1)), ExVar("x")),
									  ExCon("True"),
									  App(ExVar("even"), App(ExVar("dec"), ExVar("x")))))))

  val lengthFDnil = FunctionDef(List(PatternExpr("Nil", Nil)), ConstInt(0))

  val lengthFDcons = FunctionDef(List(PatternExpr("Cons", List(PatternVar("x"), PatternVar("xs")))),
				 App(ExVar("inc"), App(ExVar("length"), ExVar("xs"))))

  val lengthFD = ("length", List(lengthFDnil, lengthFDcons))

  val list1xFD = ("list1x", List(FunctionDef(List(PatternVar("x")),
					     App(App(ExCon("Cons"), ConstInt(1)), App(App(ExCon("Cons"), ExVar("x")), ExCon("Nil"))))))

  val foldr1FD = ("foldr1", List(FunctionDef(List(PatternVar("op"), PatternVar("xs")),
					     Case(ExVar("xs"), List(Alternative(PatternExpr("Cons", List(PatternVar("x"), PatternExpr("Nil", Nil))),
										ExVar("x")),
								    Alternative(PatternExpr("Cons", List(PatternVar("x"), PatternVar("xs"))),
										App(App(ExVar("op"), ExVar("x")), App(App(ExVar("foldr1"), ExVar("op")), ExVar("xs")))))))))

  val invalidEager = ("f", List(FunctionDef(Nil, Let(List( LetDef("ones", App(App(ExCon("Cons"), ConstInt(1)), ExVar("ones"))),
							   LetDef("head", Lambda(List(PatternExpr("Cons", List(PatternVar("x"), PatternVar("xs")))),
										 ExVar("x")))), App(ExVar("head"), ExVar("ones"))))))

  val funnyId = ("funnyId", List(FunctionDef(List(PatternVar("x")), Case(ExVar("x"), List(Alternative(PatternVar("a"), ExVar("a")))))))

  val funnyId2 = ("funnyId2", List(FunctionDef(List(PatternVar("x")),
					       Case(ExVar("x"), List(Alternative(PatternVar("a"),
										 Case(ExVar("a"),
										      List(Alternative(PatternVar("b"),
												       ExVar("b"))))))))))

  val invalidFunName = ("ord", List(FunctionDef(Nil, ConstInt(1))))

  val dupPatternVar = ("foo", List(FunctionDef(List(PatternExpr("Cons", List(PatternVar("x"), PatternVar("a"))), PatternVar("a")),
					       ExVar("a"))))


  /*
   * Signatures
   */
  val idSig = ("id", FunctionSig(FunTy(List(TyVar("a"), TyVar("a")))))

  val wrongIdSig = ("id", FunctionSig(TyExpr("Int", Nil)))

  
  /*
   * Programs
   */
  val prg01 = Program(Map(), Map(incFD, decFD, evenFD, oddFD, mapFD, lengthFD),
		      List(boolDT, pairDT, listDT, treeDT, bintreeDT))

  val prg02 = Program(Map(), Map(), allDatatypes)

  val prg03 = Program(Map(), Map(invalidEager), allDatatypes)

  val prg04 = Program(Map(), Map(invalidFunName), Nil)

  val prg05 = Program(Map(), Map(dupPatternVar), List(listDT))

  val prg06 = Program(Map(wrongIdSig), Map(idFD), allDatatypes)

  val prg07 = Program(Map(idSig), Map(idFD), allDatatypes)

  val prg08 = Program(Map(), Map(), List(dupDataConsDT))

  val prg09 = Program(Map(), Map(), List(boolDT, boolDT))

  val prg10 = Program(Map(), Map(), List(dupTypeParamDT))

  val prg11 = Program(Map(), Map(), List(undefConDT))

  val prg12 = Program(Map(), Map(), List(unusedTypeParamsDT))

  val prg13 = Program(Map(), Map(), List(bintreeDT, undefTypeParamsDT))

  val prg14 = Program(Map(), Map(), List(boolDT, wrongConsAppDT))

  val prg15 = Program(Map(), Map(invalidEager), allDatatypes)
}
