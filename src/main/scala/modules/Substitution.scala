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

import scala.collection.immutable.Set
import scala.collection.immutable.Map.{apply => _, _}


/**
  * Substitutions for type inference.
  */
trait Substitution {

  this: Syntax with Type with Context with Unification =>

  /**
    * Immutable collection of mappings from type variables to types.
    */
  type Substitution = Map[TypeVariable, Type]


  /**
    * The empty substitution.
    */
  val empty: Substitution = Map.empty


  /**
    * Domain of the substitution.
    */
  def dom(σ: Substitution) = σ.keys


  /**
    * Compose all substitutions in a given list.
    */
  def compose(substitutions: List[Substitution]) = substitutions.fold(empty)(_ <+> _)


  /**
    * Wrapper class used to define methods which can be invoked on substitutions.
    */
  implicit class RichSubstitution(σ: Substitution) {

    /**
      * Composition of substitutions, defined as
      *
      * σ <+> φ = v |-> σ(t), for φ(v) = t
      *           v |-> t   , for σ(v) = t and v is not element in the domain of φ
      */
    def <+>(that: Substitution): Substitution = {
      (σ -- dom(that)) ++ that.mapValues(this :@ _)
    }
    
    /**
      * Apply a substitution to all values of a context.
      */
    def apply(ctx: Context): Context = ctx.mapValues(this :@ _)

    /**
      * Apply a substitution to a type.
      * 
      * Note: Simply invoking 'apply' on a 'Substitution' does not convert
      * it implicitly into a 'RichSubstitution' because 'Map' already comes
      * with an 'apply' method defined on the domain of the map.
      */
    def :@(ty: Type): Type = apply(Set.empty, ty)

    /**
      * Apply a substitution to an equation.
      */
    def apply(equation: Equation): Equation = equation.map(this :@ _)

    /**
      * Apply a substitution to a list of equation.
      */
    def apply(equations: List[Equation]): List[Equation] = equations.map(apply(_))

    /**
      * Apply a substitution to a type with respect to a given set of bound variables.
      */
    def apply(boundVars: Set[TypeVariable], ty: Type): Type = ty match {
      case _: BaseType => ty

      case tv: TypeVariable => if (boundVars contains tv) ty
			       else σ.get(tv).getOrElse(ty)

      case FunctionType(s, t) => FunctionType(apply(boundVars, s), apply(boundVars, t))

      case TypeConstructor(con, types) => TypeConstructor(con, types.map(apply(boundVars, _)))

      case TypeScheme(vars, t) => TypeScheme(vars, apply(boundVars union vars.toSet, t))
    }
  }
}
