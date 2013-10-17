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
 * Unification of type terms.
 */
trait Unification {

  this: Syntax with Type with Substitution with Errors =>

  /**
   * Unification unifies the two sides of an equation.
   */
  sealed abstract class Equation {

    /**
     * Left hand side of an equation.
     */
    def leftHandSide: Type = this match {
      case Unifiable(lhs, _) => lhs
      case MoreGeneralThan(lhs, _) => lhs
    }

    /**
     * Right hand side of an equation.
     */
    def rightHandSide: Type = this match {
      case Unifiable(_, rhs) => rhs
      case MoreGeneralThan(_, rhs) => rhs
    }

    /**
     * Apply the function `f` to the left and
     * the right hand side of this equation.
     */
    def map(f: Type => Type) = this match {
      case Unifiable(lhs, rhs) => Unifiable(f(lhs), f(rhs))
      case MoreGeneralThan(lhs, rhs) => MoreGeneralThan(f(lhs), f(rhs))
    }
  }

  /**
   * Both types are unifiable.
   */
  case class Unifiable(lhs: Type, rhs: Type) extends Equation

  /**
   * Both types are unifiable and the right hand side of the
   * equation is less general than the left hand side.
   */
  case class MoreGeneralThan(lhs: Type, rhs: Type) extends Equation

  /**
   * Class providing some convenience methods to create equations.
   */
  implicit class EquationSugar(s: Type) {
    def :==:(t: Type) = Unifiable(s, t)
    def :>=:(t: Type) = MoreGeneralThan(s, t)
  }

  /**
   * Generate a list of equations based on a list of functions constructing
   * equations and the corresponding left-hand and right-hand sides.
   */
  implicit class EquationBuilder(builders: List[Function2[Type, Type, Equation]]) {
    def apply(leftHandSides: List[Type], rightHandSides: List[Type]): List[Equation] = {
      (builders, leftHandSides, rightHandSides).zipped map (_(_, _))
    }
  }

  /**
   * Find the most general unifier for the given equations.
   *
   * This implementation is basically taken from
   * "B.C. Pierce, Types and Programming Languages, 2002, p.327"
   *
   * @return If a most general unifier exists, this method returns a
   *         substitution that unifies all equations of the input.
   */
  def unify(equations: List[Equation]): Either[Error, Substitution] = equations match {
    case Nil => Right(empty)
    case e :: eqs => (e.leftHandSide, e.rightHandSide) match {
      case (s, t) if s == t => unify(eqs)

      case (tv @ TypeVariable(ide), ty) => {
        if (ty.freeVars contains tv) Left(OccursError(tv, ty))
        else {
          val σ: Substitution = Map(tv -> ty)
          unify(σ(eqs)).right.map(_ <+> σ)
        }
      }

      case (ty, tv @ TypeVariable(ide)) => e match {
        case _: Unifiable => unify(Unifiable(tv, ty) :: eqs)
        case _: MoreGeneralThan => Left(MoreGeneralError(ty, tv))
      }

      case (FunctionType(s1, s2), FunctionType(t1, t2)) => {
        unify((s1 :==: t1) :: (s2 :==: t2) :: eqs)
      }

      case (TypeConstructor(con1, s), TypeConstructor(con2, t)) => {
        if (con1 != con2) Left(MisMatchError(e.leftHandSide, e.rightHandSide))
        else {
          unify((s, t).zipped.map(_ :==: _) ++ eqs)
        }
      }

      case (TypeScheme(_, s), TypeScheme(_, t)) => e match {
        case _: Unifiable => unify(Unifiable(s, t) :: eqs)
        case _: MoreGeneralThan => unify(Unifiable(s, t) :: eqs)
      }

      case _ => Left(MisMatchError(e.leftHandSide, e.rightHandSide))
    }
  }

  /**
   * Find the most general unifier for a single equation.
   */
  def unify(equation: Equation): Either[Error, Substitution] = unify(List(equation))

  /* Unification errors */ // TODO: Better error messages
  case class OccursError(tvar: TypeVariable, ty: Type) extends Error {
    override def toString = "Occurs error: " + quote(tvar.toString) + ", " + quote(ty.toString) + ".\n"
  }
  case class MisMatchError(t1: Type, t2: Type) extends Error {
    override def toString = "Could not match "+quote(t1.toString)+" and "+quote(t2.toString)+".\n"
  }
  case class MoreGeneralError(t1: Type, t2: Type) extends Error {
    override def toString = "More general error: " + quote(t1.toString) + " and " + quote(t2.toString) +".\n"
  }
  case class UnificationError(t1: Type, t2: Type) extends Error {
    override def toString = "Unification error: " + quote(t1.toString) + " and " + quote(t2.toString) +".\n"
  }
}
