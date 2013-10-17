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

import scala.collection.immutable.List.{ tabulate }

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
    }

    /**
     * The arity of a type.
     * @return The number of arguments if this is a function type, 0 otherwise.
     */
    def arity: Int = this match {
      case FunctionType(_, t) => 1 + t.arity
      case _ => 0
    }

    /**
     * The arguments of a (possibly nested) function type.
     * @return It's arguments, if this is a (nested) function type, Nil otherwise.
     */
    def argTypes: List[Type] = this match {
      case FunctionType(s, t) => s :: t.argTypes
      case _ => Nil
    }

    /**
     * The result of a (possibly nested) function type.
     * @return It's result, if this is a (nested) function type,
     *         otherwise `resType` results to identity.
     */
    def resType: Type = this match {
      case FunctionType(_, t) => t.resType
      case _ => this
    }

    /**
     * Generalize this type with respect to an outer context,
     * i.e., bind all free type variables in a type scheme.
     */
    def generalize(ctx: Context): TypeScheme = {
      val freeVars = this.freeVars diff ctx.freeVars

      this match {
        case TypeScheme(vars, ty) => TypeScheme(vars union freeVars, ty)
        case _ => TypeScheme(freeVars, this)
      }
    }

    /**
     * Construct a function type using this type as its domain.
     */
    def -->(codom: Type) = FunctionType(this, codom)

    /**
     * Use pretty printer for the String representation.
     */
    override def toString: String = TypePrettyPrinter.pretty(this)
  }

  case class TypeVariable(ide: TypeVarName) extends Type

  case class FunctionType(s: Type, t: Type) extends Type

  case class TypeConstructor(con: TConVar, types: List[Type]) extends Type

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
    //    case TyExpr(Syntax.TConVar("Int", LocalMod), Nil, _) => BaseType(Integer)
    //    case TyExpr(Syntax.TConVar("Char", LocalMod), Nil, _) => BaseType(Character)
    //    case TyExpr(Syntax.TConVar("String", LocalMod), Nil, _) => BaseType(String)
    //    case TyExpr(Syntax.TConVar("Void", LocalMod), Nil, _) => BaseType(Void)
    //    case TyExpr(Syntax.TConVar("Real", LocalMod), Nil, _) => BaseType(Real)

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
   * Pretty printer for SL definitions and expressions.
   */
  object TypePrettyPrinter extends org.kiama.output.PrettyPrinter {

    def pretty(t: Any): String = t match {
      case t: Type => super.pretty(showType(t))
      case e => pretty_any(e)
    }

    def showType(ty: Type): Doc = ty match {
      case TypeVariable(ide) => ide
      case FunctionType(s: Type, t: Type) => parens(showType(s) <+> "->" <+> showType(t))
      case TypeConstructor(con, types) => con.toString <+> hsep(types map showType)
      case TypeScheme(vars, ty) => "forall" <+> hsep(vars map showType) <+> "." <+> showType(ty)
    }
  }
  /**
   * types of that can appear due to literal expressions.
   */
  object BaseType {
    val Integer = TypeConstructor(Syntax.TConVar("Int", LocalMod), Nil)
    val String = TypeConstructor(Syntax.TConVar("String", LocalMod), Nil)
    val Character = TypeConstructor(Syntax.TConVar("Char", LocalMod), Nil)
    val Void = TypeConstructor(Syntax.TConVar("Void", LocalMod), Nil)
    def Dom(a: Type) = TypeConstructor(Syntax.TConVar("DOM", LocalMod), List(a))
    val Real = TypeConstructor(Syntax.TConVar("Real", LocalMod), Nil)
    val Bool = TypeConstructor(Syntax.TConVar("Bool", LocalMod), Nil)
    def Lazy(a: Type) = TypeConstructor(Syntax.TConVar("LAZY", LocalMod), List(a))

    val typeVars = List(Integer, String, Character, Void, Dom(Void), Real, Bool, Lazy(Void)).map(_.con)
  }

}
