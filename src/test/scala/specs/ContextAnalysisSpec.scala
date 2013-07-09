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

import org.scalatest.matchers._
import org.scalatest.FunSpec
import de.tuberlin.uebb.sl2.modules._

trait ContextAnalysisSpec extends FunSpec with ShouldMatchers {

  this: ProgramChecker with Syntax with SLExpressions with Errors =>


  def fail(err: Error) : Matcher[Either[Error, Unit]] = be(Left(err))

  def notFail: Matcher[Either[Error, Unit]] = be(Right())

  def checking(program: AST): Either[Error, Unit] = for (_ <- checkProgram(program).right) yield ()


  describe("Context analysis: valid programs") {
    it("Should not fail on a program with some correct data type and functions definitions") {
      checking(prg01) should notFail
    }

    it("Should not fail on a program with some correct data type and no functions definitions") {
      checking(prg02) should notFail
    }

    it("Should not fail on a program with correct function definitions and signatures") {
      checking(prg07) should notFail
    }
  }

  describe("Context analysis: erroneous function definitions") {
    it("Should fail on a program using an invalid function name") {
      checking(prg04) should fail(AttributedError("Function name `ord' clashes with predefined function.", EmptyAttribute))
    }

    it("Should fail on a program with a function using duplicate pattern variables") {
      checking(prg05) should fail(ErrorList(List(AttributedError("Duplicate pattern variable `a' in function definition.", EmptyAttribute))))
    }

    it("Should fail on a program where a function and its signature do not have the same arity") {
      checking(prg06) should fail(AttributedError("Signature and definitions of `id' have different arities.", EmptyAttribute))
    }

    it("Should fail on a program with a function definition which cannot be evaluated in an eager fashion") {
      checking(prg15) should fail(AttributedError("Right-hand side of this recursive local definition is not a lambda expression.", EmptyAttribute))
    }
  }

  describe("Context analysis: erroneous data type definitions") {
    it("Should fail on a program with data type definitions using duplicate constructor names") {
      checking(prg08) should fail(ErrorList(List(DuplicateError("definitions of constructor", "DupCon", List(EmptyAttribute, EmptyAttribute)))))
    }

    it("Should fail on a program with data type definitions with duplicate type names") {
      checking(prg09) should fail(ErrorList(List(DuplicateError("type definition", "Bool", List(EmptyAttribute, EmptyAttribute)))))
    }

    it("Should fail on a program with data type definitions where type parameters are not disjoint") {
      checking(prg10) should fail(ErrorList(List(DuplicateError("type variable in type `DupTypeParam'", "a", List(EmptyAttribute)))))
    }

    it("Should fail on a program with data type definitions where undefined type constructors are used on the right-hand sides") {
      checking(prg11) should fail(AttributedError("Use of undefined type(s) in `UndefConUsed': `Undefined'", EmptyAttribute))
    }

    it("Should fail on a program with data type definitions with unused type params") {
      checking(prg12) should fail(AttributedError("Unused type variable(s) in type `UnusedTypeParam': `a'", EmptyAttribute))
    }

    it("Should fail on a program with data type definitions with undefined type params") {
      checking(prg13) should fail(AttributedError("Undefined type variable(s) in type `UndefTypeParam': `a'", EmptyAttribute))
    }

    it("Should fail on a program with data type definitions where a type constructor is applied to the wron number of arguments") {
      checking(prg14) should fail(AttributedError("Too many arguments to `Bool' in constructor `C'", EmptyAttribute))
    }
  }
}
