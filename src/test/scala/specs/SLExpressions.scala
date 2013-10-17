/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following Conditionalitions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of Conditionalitions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of Conditionalitions and the following disclaimer in the
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
 * SL expressions, definitions and patterns for testing purposes.
 */
trait SLExpressions {

  this: Syntax =>

  /*
   * Data type definitions
   */
  val boolDT = DataDef("Bool2", Nil, List(ConstructorDef("True2", Nil), ConstructorDef("False2", Nil)))

  val boolNameClashDT = DataDef("Bool", Nil, List(ConstructorDef("True2", Nil), ConstructorDef("False2", Nil)))
  val boolConstructorClashDT = DataDef("Bool2", Nil, List(ConstructorDef("True", Nil), ConstructorDef("False", Nil)))
  
  val intAPairDT = DataDef("IntAPair", List("a"), List(ConstructorDef("IntAPair", List(TyExpr(Syntax.TConVar("Int"), Nil), TyVar("a")))))

  val pairDT = DataDef("Pair", List("a", "b"), List(ConstructorDef("Pair", List(TyVar("a"), TyVar("b")))))

  val listDT = DataDef("List", List("a"), List(ConstructorDef("Nil", Nil),
					       ConstructorDef("Cons", List(TyVar("a"), TyExpr(Syntax.TConVar("List"), List(TyVar("a")))))))

  val treeDT = DataDef("Tree", List("a"), List(ConstructorDef("Null", Nil),
					       ConstructorDef("Branch", List(TyVar("a"),
									     TyExpr(Syntax.TConVar("List"), List(TyExpr(Syntax.TConVar("Tree"), List(TyVar("a")))))))))

  val bintreeDT = DataDef("BinTree", List("a"), List(ConstructorDef("Tip", Nil),
						     ConstructorDef("Node", List(TyExpr(Syntax.TConVar("BinTree"), List(TyVar("a"))),
											TyVar("a"),
											TyExpr(Syntax.TConVar("BinTree"), List(TyVar("a")))))))

  val dupDataConsDT = DataDef("DupDataCons", Nil, List(ConstructorDef("DupCon", Nil),
						     ConstructorDef("DupCon", Nil)))

  val dupTypeParamDT = DataDef("DupTypeParam", List("a", "a"), List(ConstructorDef("C", Nil)))

  val undefConDT = DataDef("UndefConUsed", Nil, List(ConstructorDef("C", List(TyExpr(Syntax.TConVar("Undefined"), Nil)))))

  val unusedTypeParamsDT = DataDef("UnusedTypeParam", List("a"), List(ConstructorDef("C", Nil)))

  val undefTypeParamsDT = DataDef("UndefTypeParam", Nil, List(ConstructorDef("C", List(TyExpr(Syntax.TConVar("BinTree"), List(TyVar("a")))))))

  val wrongConsAppDT = DataDef("WrongConsApp", List("a"), List(ConstructorDef("C", List(TyExpr(Syntax.TConVar("Bool"), List(TyVar("a")))))))

  val allDatatypes = List(boolDT, pairDT, intAPairDT, listDT, treeDT, bintreeDT)


  /*
   * Function definitions
   */
  val idFD = ("id", List(FunctionDef(List(PatternVar("x")), ExVar(Syntax.Var("x")))))

  val mapFDnil = FunctionDef(List(PatternVar("f"), PatternExpr(Syntax.ConVar("Nil"), Nil)), ExCon(Syntax.ConVar("Nil")))

  val mapFDnil1 = FunctionDef(List(PatternExpr(Syntax.ConVar("Nil"), Nil)), ExCon(Syntax.ConVar("Nil")))

  val mapFDcons = FunctionDef(List(PatternVar("f"), PatternExpr(Syntax.ConVar("Cons"), List(PatternVar("x"), PatternVar("xs")))),
			      App(App(ExCon(Syntax.ConVar("Cons")), App(ExVar(Syntax.Var("f")), ExVar(Syntax.Var("x")))),
			          App(App(ExVar(Syntax.Var("map")), ExVar(Syntax.Var("f"))), ExVar(Syntax.Var("xs")))))

  val mapFD = ("map", List(mapFDnil, mapFDcons))

  val mapFD1 = ("map", List(mapFDnil1, mapFDcons))

  val incFD = ("inc", List(FunctionDef(List(PatternVar("x")), App(App(ExVar(Syntax.Var("+")), ConstInt(1)), ExVar(Syntax.Var("x"))))))

  val decFD = ("dec", List(FunctionDef(List(PatternVar("x")), App(App(ExVar(Syntax.Var("-")), ConstInt(1)), ExVar(Syntax.Var("x"))))))

  val evenFD = ("even", List(FunctionDef(List(PatternVar("x")), Conditional(App(App(ExVar(Syntax.Var("==")), ConstInt(0)), ExVar(Syntax.Var("x"))),
									    ExCon(Syntax.ConVar("True")),
									    App(ExVar(Syntax.Var("odd")), App(ExVar(Syntax.Var("dec")), ExVar(Syntax.Var("x"))))))))

  val oddFD = ("odd", List(FunctionDef(List(PatternVar("x")), Conditional(App(App(ExVar(Syntax.Var("==")), ConstInt(1)), ExVar(Syntax.Var("x"))),
									  ExCon(Syntax.ConVar("True")),
									  App(ExVar(Syntax.Var("even")), App(ExVar(Syntax.Var("dec")), ExVar(Syntax.Var("x"))))))))

  val lengthFDnil = FunctionDef(List(PatternExpr(Syntax.ConVar("Nil"), Nil)), ConstInt(0))

  val lengthFDcons = FunctionDef(List(PatternExpr(Syntax.ConVar("Cons"), List(PatternVar("x"), PatternVar("xs")))),
				 App(ExVar(Syntax.Var("inc")), App(ExVar(Syntax.Var("length")), ExVar(Syntax.Var("xs")))))

  val lengthFD = ("length", List(lengthFDnil, lengthFDcons))

  val list1xFD = ("list1x", List(FunctionDef(List(PatternVar("x")),
					     App(App(ExCon(Syntax.ConVar("Cons")), ConstInt(1)),
					         App(App(ExCon(Syntax.ConVar("Cons")), ExVar(Syntax.Var("x"))), ExCon(Syntax.ConVar("Nil")))))))

  val foldr1FD = ("foldr1", List(FunctionDef(List(PatternVar("op"), PatternVar("xs")),
					     Case(ExVar(Syntax.Var("xs")), List(Alternative(PatternExpr(Syntax.ConVar("Cons"), List(PatternVar("x"), PatternExpr(Syntax.ConVar("Nil"), Nil))),
										ExVar(Syntax.Var("x"))),
								    Alternative(PatternExpr(Syntax.ConVar("Cons"), List(PatternVar("x"), PatternVar("xs"))),
										App(App(ExVar(Syntax.Var("op")), ExVar(Syntax.Var("x"))), App(App(ExVar(Syntax.Var("foldr1")), ExVar(Syntax.Var("op"))), ExVar(Syntax.Var("xs"))))))))))

  val invalidEager = ("f", List(FunctionDef(Nil, Let(List( LetDef("ones", App(App(ExCon(Syntax.ConVar("Cons")), ConstInt(1)), ExVar(Syntax.Var("ones")))),
							   LetDef("head", Lambda(List(PatternExpr(Syntax.ConVar("Cons"), List(PatternVar("x"), PatternVar("xs")))),
										 ExVar(Syntax.Var("x"))))), App(ExVar(Syntax.Var("head")), ExVar(Syntax.Var("ones")))))))

  val funnyId = ("funnyId", List(FunctionDef(List(PatternVar("x")), Case(ExVar(Syntax.Var("x")), List(Alternative(PatternVar("a"), ExVar(Syntax.Var("a"))))))))

  val funnyId2 = ("funnyId2", List(FunctionDef(List(PatternVar("x")),
					       Case(ExVar(Syntax.Var("x")), List(Alternative(PatternVar("a"),
										 Case(ExVar(Syntax.Var("a")),
										      List(Alternative(PatternVar("b"),
												       ExVar(Syntax.Var("b")))))))))))

  val invalidFunName = ("intToStr", List(FunctionDef(Nil, ConstInt(1))))

  val dupPatternVar = ("foo", List(FunctionDef(List(PatternExpr(Syntax.ConVar("Cons"), List(PatternVar("x"), PatternVar("a"))), PatternVar("a")),
					       ExVar(Syntax.Var("a")))))


  /*
   * Signatures
   */
  val idSig = ("id", FunctionSig(FunTy(List(TyVar("a"), TyVar("a")))))

  val wrongIdSig = ("id", FunctionSig(TyExpr(Syntax.TConVar("Int"), Nil)))

  /*
   * Programs
   */
  val prg01 = Program(List(), Map(), Map(incFD, decFD, evenFD, oddFD, mapFD, lengthFD),  Map(),
		      List(boolDT, pairDT, listDT, treeDT, bintreeDT))

  val prg02 = Program(List(), Map(), Map(), Map(), allDatatypes)

  val prg03 = Program(List(), Map(), Map(invalidEager), Map(), allDatatypes)

  val prg04 = Program(List(), Map(), Map(invalidFunName), Map(), Nil)

  val prg05 = Program(List(), Map(), Map(dupPatternVar), Map(), List(listDT))

  val prg06 = Program(List(), Map(wrongIdSig), Map(idFD), Map(), allDatatypes)

  val prg07 = Program(List(), Map(idSig), Map(idFD), Map(), allDatatypes)

  val prg08 = Program(List(), Map(), Map(), Map(), List(dupDataConsDT))

  val prg09 = Program(List(), Map(), Map(), Map(), List(boolDT, boolDT))

  val prg10 = Program(List(), Map(), Map(), Map(), List(dupTypeParamDT))

  val prg11 = Program(List(), Map(), Map(), Map(), List(undefConDT))

  val prg12 = Program(List(), Map(), Map(), Map(), List(unusedTypeParamsDT))

  val prg13 = Program(List(), Map(), Map(), Map(), List(bintreeDT, undefTypeParamsDT))

  val prg14 = Program(List(), Map(), Map(), Map(), List(boolDT, wrongConsAppDT))
    
  val prg15 = Program(List(), Map(), Map(), Map(), List(boolNameClashDT))
  
  val prg16 = Program(List(), Map(), Map(), Map(), List(boolConstructorClashDT))
}
