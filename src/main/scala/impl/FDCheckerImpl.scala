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

import scala.language.postfixOps

/**
  * Check function definitions for correctness.
  */
trait FDCheckerImpl extends FDChecker {

  this: Lexic with Syntax with Errors =>

  /**
    * Check a program's top-level function definitions.
    *
    * The following properties are checked:
    * $ - Each defining clause of a function `f' must have the same arity.
    * $ - No top-level function may have the name of a predefined function.
    * $ - All variables in patterns must be disjoint.
    * 
    * @return Definitions and signatures of all top-level functions
    */
  def checkFunctions(in: AST): Either[Error, (Map[Var, FunctionSig], Map[Var, List[FunctionDef]])] = in match {
    case Program(funSigs, funDefs, _, _) => {
      for ( _ <- checkFunctions(funDefs, funSigs).right )
      yield (funSigs, funDefs)
    }
  }  


  /**
    * Check a program's top-level function definitions.
    */
  def checkFunctions(funDefs: Map[Var, List[FunctionDef]], funSigs: Map[Var, FunctionSig]): Either[Error, Unit] = {
    for ( _ <- checkArities(funDefs, funSigs).right ;
          _ <- checkPredefClash(funDefs).right ;
	  _ <- checkDuplicatePatVars(funDefs).right )
    yield ()
  }
  
  /**
    * Check that all clauses of a function have the same arity as the signature (if given).
    */
  def checkArities(funDefs: Map[Var, List[FunctionDef]], funSigs: Map[Var, FunctionSig]): Either[Error, Unit] = {

    def checkArities(funName: Var, defs: List[FunctionDef]): Either[Error, Unit] = {
      val arguments = defs map (_.patterns.length)
      val allEqual = (args: List[Int]) => args.distinct.length == 1
      val matchingSig = funSigs.get(funName) match {
	case None      => true
	case Some(sig) => sig.typ.arity == arguments.head
      }

      if (! allEqual(arguments)) {
	val attribute = defs.head.attribute
	val message = "Definitions of " + quote(funName) + " have different arities."
	Left(AttributedError(message, attribute))
      }
      else if (! matchingSig) {
	/*
	 * If a type signature is given for this function definition, we hint on
	 * the position of the signature. Otherwise we take the position of the
	 * first function definition.
	 */
	val attribute = funSigs.get(funName).map(_.attribute).getOrElse(defs.head.attribute)
	val message = "Signature and definitions of " + quote(funName) + " have different arities."
	Left(AttributedError(message, attribute))
      }
      else Right()
    }

    for ( _ <- errorMap(funDefs.toList, checkArities _ tupled).right )
    yield ()
  }


  /**
    * Check that no function uses the name of a predefined function.
    */
  def checkPredefClash(funDefs: Map[Var, List[FunctionDef]]): Either[Error, Unit] = {
    val funNames = funDefs.keySet.toList

    def checkPredefClash(funName: Var): Either[Error, Unit] = {
      if (predefinedFuns.toSet contains funName) {
	/*
	 * We hint on the first definition of a function when
	 * its name clashes with the names of the predefined ones.
	 */
	val attribute = funDefs.get(funName).get.head.attribute
	val message = "Function name " + quote(funName) + " clashes with predefined function."
	Left(AttributedError(message, attribute))
      }
      else Right()
    }

    for ( _ <- errorMap(funNames, checkPredefClash).right )
    yield ()
  }


  /**
    * Check for duplicate pattern variables in function definitions.
    */
  def checkDuplicatePatVars(funDefs: Map[Var, List[FunctionDef]]): Either[Error, Unit] = {

    val defs: List[FunctionDef] = funDefs.values.flatten.toList
    
    def checkDuplicatePatVars(funDef: FunctionDef): Either[Error, Unit] = {
      val patternVars = funDef.patterns.flatMap(_.varsList)
      val duplicateVars = findDuplicates(patternVars)

      def handleDuplicateVars(v: Var): Error = {
	val message = "Duplicate pattern variable " + quote(v) + " in function definition."
	AttributedError(message, funDef.attribute)
      }

      if (duplicateVars.length > 0) Left(ErrorList(duplicateVars map handleDuplicateVars))
      else Right()
    }

    for ( _ <- errorMap(defs, checkDuplicatePatVars).right )
    yield ()
  }


  /**
    * Select all elements occurring two ore more times in the given list.
    */
  private def findDuplicates[T](list: List[T]): List[T] = {
    list.filter{ (e: T) => list.count(_ == e) >= 2 }.distinct
  }
}
