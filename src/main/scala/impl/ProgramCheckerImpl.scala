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

package de.tuberlin.uebb.sl2.impl

import de.tuberlin.uebb.sl2.modules._

/**
 * Program checker.
 *
 * Perform context analysis on the program's definitions.
 */
trait ProgramCheckerImpl extends ProgramChecker {

  this: Lexic with Syntax with Context with ModuleContext with ModuleResolver with Type with EnrichedLambdaCalculus with DTChecker with FDChecker with LetRecSplitter with TypeChecker with Errors =>

  /**
   * Context analysis, performing the following checks on a program:
   * $ - Data type checking,
   * $ - Eager checking for recursive let-bindings,
   * $ - Type checking of function definitions,
   * $ - Checking the type of a program's main function.
   */
  def checkProgram(in: AST, modules : List[ResolvedImport]): Either[Error, Unit] = {
    val (moduleContext, moduleSigs) = buildModuleContext(modules);
    for (
      initialContext <- checkDataTypes(in, modules).right;
      fdc <- checkFunctions(in).right ;
      _ <- { val FDCheckResult(funSigs, funDefs, externContext) = fdc ; 
            for {
              // remove local definitions from imported module context. (imported unqualified names
              // may otherwise clash with local names. this way they are shadowed.)
              elc <- splitLetRecs(moduleContext.keySet
    		                  -- funSigs.keySet.map(_.asInstanceOf[VarFirstClass])
    		                  ++ initialContext.keySet ++ externContext.keySet,
                                  programToELC(moduleSigs ++ funSigs, funDefs)).right;
        
              _ <- checkLetRecs(elc).right ;
              mainType <- {
                checkTypes(moduleContext -- funSigs.keySet.map(_.asInstanceOf[VarFirstClass])
                           ++ initialContext ++ externContext, elc).right
              }     
            } yield (checkMain(funSigs, mainType)) 
          }.right
    ) yield ()
  }

  /**
   * Check if the `main' function's type is `DOM Void'.
   */
  def checkMain(signatures: Map[Var, FunctionSig], inferredType: Type): Either[Error, Unit] = {

    val checkInferredType = if (inferredType == BaseType.Dom(BaseType.Void)) {
      Right()
    } else {
      Left(GenericError("Function `main' must be of type `DOM Void', but found: " + quote(inferredType.toString)))
    }

    val checkMainSignature = signatures.get(Syntax.Var("main")) match {
      case None => Right()
      case Some(FunctionSig(signature, modi, attr)) => {
        val mainSignature = astToType(signature)
        if (mainSignature == inferredType) Right()
        else Left(AttributedError("Could not match declared type " + quote(mainSignature.toString) + " against inferred type " + quote(inferredType.toString) + " in `main'", attr))
      }
    }

    for (
      _ <- checkInferredType.right;
      _ <- checkMainSignature.right
    ) yield ()
  }

  /**
* Check if all right-hand sides in recursive let-bindings are lambda-abstractions, i.e,
* they can be evaluated in an eager fashion.
*/
  def checkLetRecs(expr: ELC): Either[Error, Unit] = {

    /**
* Check if the right-hand side of a local definition is a lambda-abstraction or a choice-expression.
*/
    def rhsIsLamOrChoice(definition: EDefinition): Either[Error, Unit] = definition match {
      case EDefinition(_, _, ELam(_, _, _), _) => Right()
      case EDefinition(_, _, EChoice(_, _), _) => Right()
      case EDefinition(_, _, _, attr) => {
val errorMsg = "Right-hand side of this recursive local definition is not a lambda expression."
Left(AttributedError(errorMsg, attr))
      }
    }

    /* Recursive traversal over the ELC expression */
    expr match {
      case ELetRec(defs, body, _) => for ( _ <- errorMap(defs, (d: EDefinition) => rhsIsLamOrChoice(d)).right ;
_ <- checkLetRecs(body).right )
yield ()

      case EApp(fun, expr, _) => for ( _ <- checkLetRecs(fun).right ;
_ <- checkLetRecs(expr).right )
yield ()

      case ELam(_, body, _) => for ( _ <- checkLetRecs(body).right ) yield ()

      case ELet(definition, body, _) => for ( _ <- checkLetRecs(definition.rhs).right ;
_ <- checkLetRecs(body).right )
yield ()

      case EChoice(choices, _) => for ( _ <- errorMap(choices, (e: ELC) => checkLetRecs(e)).right ) yield ()

      case ECase(expr, alts, _) => for ( _ <- checkLetRecs(expr).right ;
_ <- errorMap(alts.map(_.expr), (e: ELC) => checkLetRecs(e)).right )
yield ()

      case _ => Right()
    }
  }
}

