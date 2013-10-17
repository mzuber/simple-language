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

import scala.language.postfixOps

/**
 * Check function definitions for correctness.
 */
trait FDCheckerImpl extends FDChecker {

  this: Lexic with Syntax with Context with Type with Errors =>

  /**
   * Check a program's top-level function definitions.
   *
   * The following properties are checked:
   * $ - Each defining clause of a function `f' must have the same arity.
   * $ - All variables in patterns must be disjoint.
   *
   * @return Definitions and signatures of all top-level functions
   */
  override def checkFunctions(in: AST):
	  Either[Error, FDCheckResult] = in match {
    case Program(imports, funSigs, funDefs, funDefsExtern, _, _) => {
      val funQualifiedDefs = funDefs.toList.map({case (name,body) => (Syntax.Var(name), body)}).toMap
      val funQualifiedSigs = funSigs.toList.map({case (name,sig) => (Syntax.Var(name), sig)}).toMap
      val funQualifiedDefsExt = funDefsExtern.toList.map({case (name,body) => (Syntax.Var(name), body)}).toMap
      val undeclaredExterns = funDefsExtern.filter{case (name, eDef) => !funSigs.isDefinedAt(name)}
      if (!undeclaredExterns.isEmpty) {
        val undeclaredWittness = undeclaredExterns.head
        Left(AttributedError("External definition of " + quote(undeclaredWittness._1) +
            " lacks an explicit declaration.", undeclaredWittness._2.attribute))
      } else {
      val externContext: Context = funDefsExtern.map {
          case (name, eDef) => (Syntax.Var(name).asInstanceOf[VarFirstClass] ->
            astToType(funSigs(name).typ))
        }
      for (_ <- checkFunctions(funQualifiedDefs, funQualifiedDefsExt, funQualifiedSigs).right)
        yield FDCheckResult(funQualifiedSigs, funQualifiedDefs, externContext)
      }
    }
  }

  def checkCompleteImplementation(funDefs: Map[Var, List[FunctionDef]], funDefsExt: Map[Var, FunctionDefExtern], funSigs: Map[Var, FunctionSig]): Either[Error, Unit] = {
    val unimplemented = funSigs -- funDefs.keySet -- funDefsExt.keySet
    def mkErrors(funSigs: Map[Var, FunctionSig]): List[Error] =
      for ( sig <- funSigs.toList ) yield (sig match {
        case (name, FunctionSig(_, _, attr)) =>
          NotImplementedError("unimplemented function signature", name.toString, attr)
      })

    if (unimplemented.isEmpty)
      Right()
    else
      Left(ErrorList(mkErrors(unimplemented).toList))
  }

  /**
   * Check a program's top-level function definitions.
   */
  def checkFunctions(funDefs: Map[Var, List[FunctionDef]], funDefsExt: Map[Var, FunctionDefExtern], funSigs: Map[Var, FunctionSig]): Either[Error, Unit] = {
    for (
      _ <- checkArities(funDefs, funSigs).right;
      _ <- checkDuplicatePatVars(funDefs).right;
      _ <- checkCompleteImplementation(funDefs, funDefsExt, funSigs).right
    ) yield ()
  }

  /**
   * Check that all clauses of a function have the same arity as the signature (if given).
   */
  def checkArities(funDefs: Map[Var, List[FunctionDef]], funSigs: Map[Var, FunctionSig]): Either[Error, Unit] = {

    def checkArities(funName: Var, defs: List[FunctionDef]): Either[Error, Unit] = {
      val arguments = defs map (_.patterns.length)
      val allEqual = (args: List[Int]) => args.distinct.length == 1
      val matchingSig = funSigs.get(funName) match {
        case None => true
        case Some(sig) => sig.typ.arity == arguments.head
      }

      if (!allEqual(arguments)) {
        val attribute = defs.head.attribute
        val message = "Definitions of " + quote(funName.toString) + " have different arities."
        Left(AttributedError(message, attribute))
      } else if (!matchingSig) {
	    /*
		 * If a type signature is given for this function definition, we hint on
		 * the position of the signature. Otherwise we take the position of the
		 * first function definition.
		 */
        val attribute = funSigs.get(funName).map(_.attribute).getOrElse(defs.head.attribute)
        val message = "Signature and definition of "+ quote(funName.toString) +" have different arities."
        Left(AttributedError(message, attribute))
      } else Right()
    }

    for (_ <- errorMap(funDefs.toList, checkArities _ tupled).right)
      yield ()
  }

  /**
   * Check for duplicate pattern variables in function definitions.
   */
  def checkDuplicatePatVars(funDefs: Map[Var, List[FunctionDef]]): Either[Error, Unit] = {

    val defs: List[FunctionDef] = funDefs.values.flatten.toList

    def checkDuplicatePatVars(funDef: FunctionDef): Either[Error, Unit] = {
      val patternVars = funDef.patterns.flatMap(varsList)
      val duplicateVars = findDuplicates(patternVars)

      def handleDuplicateVars(v: VarName): Error = {
        val message = "Duplicate pattern variable " + quote(v) + " in function definition."
        AttributedError(message, funDef.attribute)
      }

      if (duplicateVars.length > 0) Left(ErrorList(duplicateVars map handleDuplicateVars))
      else Right()
    }

    for (_ <- errorMap(defs, checkDuplicatePatVars).right)
      yield ()
  }

  /**
   * Select all elements occurring two ore more times in the given list.
   */
  private def findDuplicates[T](list: List[T]): List[T] = {
    list.filter { (e: T) => list.count(_ == e) >= 2 }.distinct
  }
}
