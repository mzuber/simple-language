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

package de.tuberlin.uebb.sl2.impl

import scala.io.Source

import de.tuberlin.uebb.sl2.modules._
import de.tuberlin.uebb.sl2.modules.Syntax.Var

/**
  * The SL front-end.
  */
object FrontEndImpl extends FrontEnd with Lexic with Syntax with CombinatorParser
		    with EnrichedLambdaCalculus with GraphImpl[Var] with LetRecSplitter with Type
		    with NameSupply with Context with Substitution with Unification
		    with TypeCheckerImpl with DTCheckerImpl with FDCheckerImpl
		    with ProgramCheckerImpl with Errors {


  def main(args: Array[String]) {
    val usage = """Usage: <sl> file(s)"""

    if (args.isEmpty)
      println(usage)
    else {
      val input = args map { f =>
        val source = scala.io.Source.fromFile(f)
        val code = source.mkString
        source.close()
        code
      }
      val prelude = Source.fromURL(getClass.getResource("/prelude.sl")).getLines.mkString("\n")

      run(prelude +: input.toList).fold(println(_), println(_))
    }
  }
  
}
