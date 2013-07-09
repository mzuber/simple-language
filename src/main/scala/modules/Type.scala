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

import scala.collection.immutable.List.{tabulate}

/**
  * SL types.
  */
trait Type {

  this: Syntax with EnrichedLambdaCalculus with Context with Substitution with NameSupply =>

  /**
    * SL type language.
    */
  sealed abstract class Type {
    
    /**
      * All type variables occuring free, i.e., not bound
      * by the quantifier of a type scheme, in this type.
      */
    def freeVars: List[TypeVariable] = freeVars(Nil).distinct

    /**
      * All type variables occuring free in this type with
      * respect to a given list of bound type variables.
      */
    def freeVars(boundVars: List[TypeVariable]): List[TypeVariable] = this match {
      case tv: TypeVariable => if (boundVars contains tv) Nil else List(tv)

      case FunctionType(s, t) => s.freeVars(boundVars) ++ t.freeVars(boundVars)

      case TypeConstructor(_, tys) => {
	def collect(t: Type, fvs: List[TypeVariable]) = t.freeVars(boundVars) ++ fvs
	val start: List[TypeVariable] = Nil
	tys.foldRight(start)(collect)
      }

      case TypeScheme(tvs, ty) => ty.freeVars(tvs)

      case _: BaseType => Nil
    }

    /**
      * The arity of a type.
      * @return The number of arguments if this is a function type, 0 otherwise.
      */
    def arity: Int = this match {
      case FunctionType(_, t) => 1 + t.arity
      case _                  => 0
    }

    /**
      * The arguments of a (possibly nested) function type.
      * @return It's arguments, if this is a (nested) function type, Nil otherwise.
      */
    def argTypes: List[Type] = this match {
      case FunctionType(s, t) => s :: t.argTypes
      case _                  => Nil
    }

    /**
      * The result of a (possibly nested) function type.
      * @return It's result, if this is a (nested) function type,
      *         otherwise `resType` results to identity.
      */
    def resType: Type = this match {
      case FunctionType(_, t) => t.resType
      case _                  => this
    }

    /**
      * Generalize this type with respect to an outer context,
      * i.e., bind all free type variables in a type scheme.
      */
    def generalize(ctx: Context): TypeScheme = {
      val freeVars = this.freeVars diff ctx.freeVars

      this match {
	case TypeScheme(vars, ty) => TypeScheme(vars union freeVars, ty)
	case _                    => TypeScheme(freeVars, this)
      }
    }

    /**
      * Construct a function type using this type as its domain.
      */
    def -->(codom: Type) = FunctionType(this, codom)

    /**
      * Use pretty printer for the String representation. 
      */
    override def toString: String = pprint(this)
  }

  case class TypeVariable(ide: TypeVar) extends Type

  case class FunctionType(s: Type, t: Type) extends Type

  case class BaseType(b: Base) extends Type

  case class TypeConstructor(con: ConVar, types: List[Type]) extends Type

  case class TypeScheme(vars: List[TypeVariable], ty: Type) extends Type {

    /**
      * Instantiate a type scheme by replacing all bound
      * type variables with freshly generated ones.
      */
    def instantiate: Type = {
      val σ: Substitution = vars.zip(freshTVars(vars.length)).toMap
      σ :@ ty
    }
  }

  def forall(vars: TypeVariable*)(ty: Type) = TypeScheme(vars.toList, ty)


  /**
    * SL base types: Int, Char, String, and Void.
    */
  sealed abstract class Base
  case object Integer extends Base
  case object Character extends Base
  case object String extends Base

  /**
    * Generate a fresh type variable.
    */
  def freshTVar(): TypeVariable = TypeVariable(freshName())


  /**
    * Generate a list of `n` fresh type variables.
    */
  def freshTVars(n: Int): List[TypeVariable] = tabulate(n)(_ => freshTVar())


  /**
    * Build types from abstract syntax.
    */
  def astToType(astType: ASTType): Type = astType match {
    /* Base types: Int, Char, String, Void */
    case TyExpr("Int", Nil, _)    => BaseType(Integer)
    case TyExpr("Char", Nil, _)   => BaseType(Character)
    case TyExpr("String", Nil, _) => BaseType(String)

    /* Function types */
    case FunTy(types, _) => {
      types.map(astToType).reduceRight(FunctionType)
    }

    /* Type constructor */
    case TyExpr(tcon, types, _) => TypeConstructor(tcon, types.map(astToType))

    /* Type variables */
    case TyVar(ide, _) => TypeVariable(ide)
  }

  /**
    * Pretty printer for type terms.
    * TODO: Use Kiama pretty printer
    */
  def pprint(ty: Type): String = ty match {
    case BaseType(Integer)   => "Int"
    case BaseType(Character) => "Char"
    case BaseType(String)    => "String"

    case TypeVariable(ide) => ide

    case FunctionType(s: Type, t: Type) => "(" + pprint(s) + " -> " + pprint(t) + ")"

    case TypeConstructor(con, types) => con + " " + (types map pprint)

    case TypeScheme(vars, ty) => "forall " + (vars map pprint) + " . " + pprint(ty)
  }
}
