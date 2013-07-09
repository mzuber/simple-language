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

package de.tuberlin.uebb.sl2.modules

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.runtime.universe._

/**
  * Generic traversal of an abstract syntax tree.
  */
trait SyntaxTraversal {

  this: Syntax =>

  /*
   * Since scala does not support rank 2 polymorphism,
   * this trait is used as workaround.
   * The type of the argument for the apply method is not known,
   * until it is called. The type Id[A] = A is
   * used to allow functions A -> A as arguments, wrapped in the ~> trait. 
   * 
   */
  type Id[A] = A

  trait ~>[F[_], G[_]] {
    def apply[A](a: F[A]): G[A]
    def isDefinedAt[A: TypeTag](a: A): Boolean
  }


  /**
    * Wraps a partial function, to use it as an argument for the map function.
    * 'isDefinedAt' checks whether the function argument matches the functions parameter type.
    */
  implicit def particalFunctionToFunctorTrans[B: TypeTag](f: PartialFunction[B, B]) = new (Id ~> Id) {
    def apply[A](a: A): A = f(a.asInstanceOf[B]).asInstanceOf[A]
    def isDefinedAt[A: TypeTag](a: A): Boolean = typeTag[A].tpe <:< typeTag[B].tpe && f.isDefinedAt(a.asInstanceOf[B])
  }


  /**
    * Generic map function for SL definitions and expressions.
    */
  def map[A: TypeTag](f: Id ~> Id, x: A): A = {
    val y = x match {
      case Program(signatures, functionDefs, dataDefs, a) => {
        val sigs: Map[Var, FunctionSig] = signatures.map { case (a, b) => map(f, (map(f, a), map(f, b))) }
        val fund: Map[Var, List[FunctionDef]] = functionDefs.map { case (a, b) => map(f, (map(f, a), map(f, b.map(map(f, _))))) }
        Program(map(f, sigs), map(f, fund), map(f, dataDefs.map(map(f, _))), map(f, a))
      }

      case FunctionSig(typ, a) => FunctionSig(map(f, typ), map(f, a))

      case FunctionDef(patterns, expr, a) => FunctionDef(map(f, patterns.map(map(f, _))), map(f, expr), map(f, a))

      case PatternVar(ide, a) => PatternVar(map(f, ide), map(f, a))

      case PatternExpr(con, patExprs, a) => PatternExpr(map(f, con), map(f, patExprs.map(map(f, _))), map(f, a))

      case DataDef(ide, tvars, constructors, a) => DataDef(map(f, ide), map(f, tvars.map(map(f, _))), map(f, constructors.map(map(f, _))), map(f, a))

      case ConstructorDef(constructor, types, a) => ConstructorDef(map(f, constructor), map(f, types.map(map(f, _))), map(f, a))

      case TyVar(ide, a) => TyVar(map(f, ide), map(f, a))

      case FunTy(types, a) => FunTy(map(f, types.map(map(f, _))), map(f, a))

      case TyExpr(conType, typeParams, a) => TyExpr(map(f, conType), map(f, typeParams.map(map(f, _))), map(f, a))

      case Conditional(c, t, e, a) => Conditional(map(f, c), map(f, t), map(f, e), map(f, a))

      case Lambda(ps, e, a) => Lambda(map(f, ps.map(map(f, _))), map(f, e), map(f, a))

      case Case(e, as, a) => Case(map(f, e), map(f, as.map(map(f, _))), map(f, a))

      case Let(ds, e, a) => Let(map(f, ds.map(map(f, _))), map(f, e), map(f, a))

      case App(g, e, a) => App(map(f, g), map(f, e), map(f, a))

      case ExVar(i, a) => ExVar(map(f, i), map(f, a))

      case ExCon(c, a) => ExCon(map(f, c), map(f, a))

      case ConstInt(v, a) => ConstInt(map(f, v), map(f, a))

      case ConstChar(c, a) => ConstChar(map(f, c), map(f, a))

      case ConstString(s, a) => ConstString(map(f, s), map(f, a))

      case Alternative(pattern, expr, a) => x

      case LetDef(lhs, rhs, a) => x

      case a => a
    }

    if (f.isDefinedAt(x))
      f(y).asInstanceOf[A]
    else
      y.asInstanceOf[A]
  }
}
