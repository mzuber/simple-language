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
  * Alpha conversion for SL expressions.
  */
trait AlphaConversion {

  this: Syntax =>

  type NewName = Unit => String

  def substitute(fresh : NewName, subst : Map[VarName, VarName], a : Alternative) : Alternative = {
    val bound = vars(a.pattern)
    val rename = bound.intersect(subst.values.toSet)
    if (rename.isEmpty) {
      Alternative(a.pattern, substitute(fresh, subst -- vars(a.pattern), a.expr))
    } else {
      /* alpha conversion */
      val renameMap = Map() ++ (rename map (r => (r -> fresh())))
      substitute(fresh, subst, substitute(fresh, renameMap, a))
    }
  }

  def substitute(subst : Map[VarName, VarName], pattern : Pattern) : Pattern = pattern match {
    case PatternExpr(c, patterns, _) => PatternExpr(c, patterns map (substitute(subst, _)))
    case PatternVar(x, _) if subst.contains(x) => PatternVar(subst(x))
    case p@_ => p
  }

  def substitute(fresh : NewName, subst : Map[VarName, VarName], e : Expr) : Expr = e match {

    case Conditional(i, t, e, attr) => Conditional(substitute(fresh, subst, i), 
                                                   substitute(fresh, subst, t), 
                                                   substitute(fresh, subst, e), attr)
 
    case Lambda(pats, e, attr) => {
      val bound = pats.flatMap(vars).toSet
      val rename = bound.intersect(subst.values.toSet)
      if (rename.isEmpty) {
        Lambda(pats, substitute(fresh, subst -- bound, e), attr) 
      } else {
        /* alpha conversion */
        val renameMap = Map() ++ (rename map (r => (r -> fresh())))
        substitute(fresh, subst, Lambda(pats map (substitute(renameMap, _)), substitute(fresh, renameMap, e)))
      }
    }

    case Case(e1, alts, attr) => {
      Case(substitute(fresh, subst, e1), alts map {a => substitute(fresh, subst, a)}, attr)
    }

    case Let(defs, rhs, attr) => {
      val bound = (defs map (_.lhs)).toSet
      val rename = bound.intersect(subst.values.toSet)
      if (rename.isEmpty) {
        Let(defs map {d => d.copy(rhs = substitute(fresh, subst -- bound, d.rhs))}, substitute(fresh, subst -- bound, rhs))
      } else {
        val renameMap = Map() ++ (rename map (r => (r -> fresh())))
        val l2 = Let(defs map {d => d.copy(rhs = substitute(fresh, renameMap, d.rhs))}, substitute(fresh, renameMap, rhs))
        substitute(fresh, subst, l2)
      }
    }

    case App(l, r, attr) => App(substitute(fresh, subst, l), substitute(fresh, subst, r), attr)
    case ExVar(Syntax.Var(x,Syntax.LocalMod), attr) => ExVar(Syntax.Var(subst.get(x).getOrElse(x)), attr)
    case c@ExVar(_, _) => c
    case c@ExCon(_, _) => c
    case c@ConstInt(_, _) => c
    case c@ConstReal(_, _) => c
    case c@ConstChar(_, _) => c
    case c@ConstString(_, _) => c
    case c@JavaScript(_, _, _) => c
  }
}
