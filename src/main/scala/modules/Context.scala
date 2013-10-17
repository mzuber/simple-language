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
 * Environment for type assumptions.
 */
trait Context {

  this: Syntax with Type =>

  type Context = Map[VarFirstClass, Type]

  /**
   * Wrapper class used to define methods which can be invoked on contexts.
   */
  implicit class RichContext(ctx: Context) {

    /**
     * Combine two contexts. Bindings of this context shadow
     * bindings of the other one in case of identical identifiers.
     */
    def <++>(that: Context): Context = that ++ ctx

    /**
     * Look up the type of an identifier in the given type environment.
     *
     * If the type associated with the given identifier is a type scheme,
     * it will be instantiated accordingly.
     */
    def lookupFresh(ide: VarFirstClass): Option[Type] = {

      def instantiate(t: Type) = t match {
        case ts: TypeScheme => ts.instantiate
        case _ => t
      }

      ctx.get(ide) map instantiate
    }

    /**
     * Determine all free variables of all type terms in a context.
     */
    def freeVars: List[TypeVariable] = {
      ctx.values.map(_.freeVars).flatten.toList.distinct
    }

    /**
     * Look up a list of identifiers in this context.
     * @return A list of the corresponding types, if and only if
     *         this context contains all of the given identifiers.
     */
    def lookupList(vars: List[VarFirstClass]): Option[List[Type]] = vars match {
      case Nil => Some(Nil)
      case v :: vs => for (
        t <- ctx.get(v);
        ts <- lookupList(vs)
      ) yield (t :: ts)
    }
  }

  /**
   * Combine a list of contexts.
   */
  def join(contexts: List[Context]): Context = contexts.fold(Map.empty)(_ <++> _)
}
