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
 * */

package de.tuberlin.uebb.sl2.impl

import de.tuberlin.uebb.sl2.modules._
import scala.collection.immutable.List.{ fill }

/**
 * Check data type definitions for correctness.
 */
trait DTCheckerImpl extends DTChecker
	with Lexic
	with Syntax
	with Context
	with Type
	with NameSupply
	with EnrichedLambdaCalculus
	with Substitution
	with Unification
	with Errors {
  
  this: ModuleResolverImpl =>

  /**
   * Check all data type definitions in a program.
   *
   * The following conditions are checked:
   * $ - The names of all type constructors must be disjoint.
   * $ - The names of all data constructors must be disjoint.
   * $ - The names of all type variables in a DATA definition must be disjoint.
   * $ - The right-hand side(s) of of a DATA definition must not mention any type
   *     variable not introduced on the left-hand side.
   * $ - All type operators used in data constructor declarations must be defined
   *     by some DATA definition.
   * $ - All type operators must be fully applied, i.e., form a proper type, in
   *     all data constructor definitions.
   *
   * @return The initial context containing all data constructors
   */
  def checkDataTypes(in: AST, imports : List[ResolvedImport]): Either[Error, Context] = in match {
    case Program(_, _, _, _, dataDefs, _) => checkDataTypes(dataDefs, imports)
  }

  /**
   * Check data type definitions and return initial
   * context containing all data constructors.
   */
  def checkDataTypes(dataDefs: List[DataDef], imports : List[ResolvedImport]): Either[Error, Context] = {
    val unqualifiedImportedDataDefs = imports.map{
      case rimp:ResolvedUnqualifiedImport => rimp.signature.dataDefs
      case _ => List()
    }.flatten
    for (
      _ <- checkTypeConsDisjoint(dataDefs ++ unqualifiedImportedDataDefs).right;
      _ <- checkDataConsDisjoint(dataDefs ++ unqualifiedImportedDataDefs).right;
      _ <- checkTypeParamsDisjoint(dataDefs).right;
      _ <- checkNoUndefinedTypeCons(dataDefs, imports).right;
      _ <- checkTypeVarUsage(dataDefs).right;
      _ <- checkTypeConsApp(dataDefs, imports).right
    ) yield dataConTypes(dataDefs)
  }

  /**
   * Check that the names of all type constructors are disjoint.
   */
  def checkTypeConsDisjoint(dataDefs: List[DataDef]): Either[Error, Unit] = {
    val typeConstructors = allTypeCons(dataDefs)
    
    def handleDuplicate(tcon: TConVar) = {
      val duplicateDefs = dataDefs filter {
        (dataDef: DataDef) => tcon == Syntax.TConVar(dataDef.ide)
      }
      val locations = duplicateDefs map (_.attribute)
      DuplicateError("type definition", tcon.toString, locations)
    }

    findDuplicates(typeConstructors) match {
      case Nil => Right()
      case dups => Left(ErrorList(dups map handleDuplicate))
    }
  }

  /**
   * Check that the names of all data constructors are disjoint.
   */
  def checkDataConsDisjoint(dataDefs: List[DataDef]): Either[Error, Unit] = {

    def handleDuplicate(con: ConVar) = {
      val constructorDefs = dataDefs flatMap (_.constructors)
      val duplicateConsDefs = constructorDefs filter {
        (consDef: ConstructorDef) => con == Syntax.ConVar(consDef.constructor)
      }
      val locations = duplicateConsDefs map (_.attribute)
      DuplicateError("definitions of constructor", con.toString, locations)
    }

    findDuplicates(allDataCons(dataDefs)) match {
      case Nil => Right()
      case dups => Left(ErrorList(dups map handleDuplicate))
    }
  }

  /**
   * Check that all type parameters in a type definition are disjoint.
   */
  def checkTypeParamsDisjoint(dataDefs: List[DataDef]): Either[Error, Unit] = {

    def checkTypeParamsDisjoint(dataDef: DataDef) = {
      val handleDuplicate = (tv: TypeVarName) =>
        DuplicateError("type variable in type " + quote(dataDef.ide), tv, List(dataDef.attribute))

      findDuplicates(dataDef.tvars) match {
        case Nil => Right()
        case dups => Left(ErrorList(dups map handleDuplicate))
      }
    }

    for (_ <- errorMap(dataDefs, checkTypeParamsDisjoint).right)
      yield ()
  }

  /**
   * Check that no undefined type constructors are used in any right-hand side.
   */
  def checkNoUndefinedTypeCons(dataDefs: List[DataDef], imports : List[ResolvedImport]): Either[Error, Unit] = {
    val localTypeConstructors = (allTypeCons(dataDefs)).toSet
    val importedTypeConstructors = imports.map(_ match {
      case rimp : ResolvedQualifiedImport => allTypeCons(rimp.signature.dataDefs, rimp.name).toSet
      case rimp : ResolvedUnqualifiedImport => allTypeCons(rimp.signature.dataDefs).toSet
      case _ => Set()
    }).reduce(_ ++ _)
    
    val typeConstructors = localTypeConstructors ++ importedTypeConstructors

    def checkType(dataDef: DataDef): Either[Error, Unit] = {
      val rhsConstructors = allAppTypeCons(dataDef.constructors).toSet

      if (!rhsConstructors.subsetOf(typeConstructors)) {
        val undefinedTypes = rhsConstructors diff typeConstructors
        val message = "Use of undefined type(s) in " + quote(dataDef.ide) + ": " + quote(undefinedTypes.map(_.toString))
        Left(AttributedError(message, dataDef.attribute))
      } else Right()
    }

    for (_ <- errorMap(dataDefs, checkType).right)
      yield ()
  }

  /**
   * Check for unused and undefined type variables in a data type definition.
   */
  def checkTypeVarUsage(dataDefs: List[DataDef]): Either[Error, Unit] = {

    def selectTypeVars(ty: ASTType): List[TypeVarName] = ty match {
      case TyVar(ide, _) => List(ide)
      case FunTy(types, _) => types flatMap selectTypeVars
      case TyExpr(_, params, _) => params flatMap selectTypeVars
    }

    def checkTypeVarUsage(dataDef: DataDef) = {
      val lhsTypeVars = dataDef.tvars.toSet
      val rhsTypeVars = dataDef.constructors.flatMap(_.types).flatMap(selectTypeVars).toSet

      if (dataDef.constructors.isEmpty || lhsTypeVars == rhsTypeVars) {
        Right()
      } else {
        val message =
          // Undefined type variables in the constructor definitions
          if (lhsTypeVars subsetOf rhsTypeVars) {
            val undefinedTypeVars = rhsTypeVars diff lhsTypeVars
            "Undefined type variable(s) in type " + quote(dataDef.ide) + ": " + quote(undefinedTypeVars)
          } // Unused typed variables in the data type definition
          else {
            val unusedTypeVars = lhsTypeVars diff rhsTypeVars
            "Unused type variable(s) in type " + quote(dataDef.ide) + ": " + quote(unusedTypeVars)
            // Note that for data defs with no constructors it is acceptable to have unused type vars,
            // as these types are constrcuted using EXTERN features.
          }
        Left(AttributedError(message, dataDef.attribute))
      }
    }

    for (_ <- errorMap(dataDefs, checkTypeVarUsage).right)
      yield ()
  }

  /**
   * Check if all type constructors are applied to enough arguments.
   */
  def checkTypeConsApp(dataDefs: List[DataDef], imports : List[ResolvedImport]): Either[Error, Unit] = {
    val importedConArities: List[(TConVar, Int)] = imports.map{
      case rimp : ResolvedQualifiedImport =>
        rimp.signature.dataDefs.map{dataDef => ((Syntax.TConVar(dataDef.ide, rimp.name), dataDef.tvars.length))}
      case rimp : ResolvedUnqualifiedImport =>
        rimp.signature.dataDefs.map{dataDef => ((Syntax.TConVar(dataDef.ide), dataDef.tvars.length))}
      case _ => List()
    }.flatten
    
    val constructorArities: Map[TConVar, Int] = {
      val constructorArity = (dataDef: DataDef) => (Syntax.TConVar(dataDef.ide), dataDef.tvars.length)
      (dataDefs.map(constructorArity) ++ importedConArities).toMap
    }

    def checkTypeConsApp(dataDef: DataDef) = {
      // TODO: Could this be done in one for comprehension?
      def checkConstructor(conDef: ConstructorDef) = {
        val con = conDef.constructor
        for (_ <- errorMap(conDef.types, (ty: ASTType) => checkArity(Syntax.ConVar(con), ty)).right)
          yield ()
      }

      for (_ <- errorMap(dataDef.constructors, checkConstructor).right)
        yield ()
    }

    def checkArity(con: ConVar, ty: ASTType): Either[Error, Unit] = ty match {
      case TyVar(_, _) => Right()

      case FunTy(tys, _) => {
        for (_ <- errorMap(tys, (ty: ASTType) => checkArity(con, ty)).right)
          yield ()
      }

      case TyExpr(tyCon, args, attr) => {
        val tyConArity = constructorArities.get(tyCon).get

        // Too few arguments in type constructor application
        if (args.length < tyConArity) {
          val message = "Too few arguments to " + quote(tyCon.toString) + " in constructor " + quote(con.toString)
          Left(AttributedError(message, attr))
        } // Too many arguments in type constructor application
        else if (args.length > tyConArity) {
          val message = "Too many arguments to " + quote(tyCon.toString) + " in constructor " + quote(con.toString)
          Left(AttributedError(message, attr))
        } else Right()
      }
    }

    for (_ <- errorMap(dataDefs, checkTypeConsApp).right)
      yield ()
  }

  /**
   * Calculate the types of the data constructors, i.e., the initial context.
   */
  def dataConTypes(dataDefs: List[DataDef], module: ModuleVar = Syntax.LocalMod): Context = {
    val ctx = for (
      dataDef <- dataDefs;
      conDef <- dataDef.constructors
    ) yield {
      val resultType = TypeConstructor(Syntax.TConVar(dataDef.ide, module), dataDef.tvars map TypeVariable)
      val argTypes = conDef.types map astToType
      val conType = argTypes.:+(resultType) reduceRight FunctionType

      // Generalize type over its free variables
      val conTypeScheme = TypeScheme(conType.freeVars, conType)
      (Syntax.ConVar(conDef.constructor, module) -> conTypeScheme)
    }

    ctx.toMap
  }

  /**
   * Select all elements occurring two ore more times in the given list.
   */
  private def findDuplicates[T](list: List[T]): List[T] = {
    list.filter { (e: T) => list.count(_ == e) >= 2 }.distinct
  }
}
