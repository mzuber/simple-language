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

import java.io.File
import java.net.URL

/**
  * The module defining the structured error representation.
  */
trait Errors {

  self: Syntax =>
  
  abstract class Error extends Throwable {
    /**
      * Produce an error message.
      */
    def message: String = "error"

    /**
      * Combine two errors into an error list.
      */
    def <+>(that: Error): ErrorList = ErrorList(List(this, that))
    
    override def toString(): String = errorMessage(this)
  }

  /* Generic error with a static error message */
  case class GenericError(what: String) extends Error {
    override def message = what
  }

  /* If a phase depends on another phase that produced an error, forward that error. */
  case class CouldNotRun(what : String, why : Error) extends Error {
    override def message = "Could not run %s: %s".format(what, why.message)
  }

  /* Groups of errors */
  case class ErrorList(errors : List[Error]) extends Error {
    override def message = errors.map(_.message).mkString("\n")
  }
  
  /* Useful during implementation */
  case object NotYetImplemented extends Error {
    override def message = "This feature is not yet implemented."
  }

  /* Parser: parse error */
  case class ParseError(what: String, where: Attribute) extends Error
  
  /* Import Checker: path not well formed */
  case class InvalidPathError(what : String, where : Attribute) extends Error

  /* Import Checker: multiple imports from the same source */
  case class DuplicatePathError(what : String, where : Attribute) extends Error

  /* Import Checker: clashing module identifiers */
  case class DuplicateModuleError(what : String, where : Attribute) extends Error
  
  /* Multi driver: circular dependency */
  case class CircularDependencyError(what: String) extends Error
  
  /* Multi driver: file not found */
  case class FileNotFoundError(file: String) extends Error
  case class FilesNotFoundError(details: String, file1: String, file2: String) extends Error
  
  /* Type checker: undefined element error */
  case class UndefinedError(what: String, name: String, where: Attribute) extends Error {
    override def message = "Undefined %s '%s'".format(what, name)
  }

  /* Type checker: type error */
  case class TypeError(what: String, where: Attribute, cause: Error) extends Error {
    override def message = "Type error in %s: %s".format(what, cause.message)
  }

  /* Context analysis: duplicate error */
  case class DuplicateError(what: String, name: String, where: List[Attribute]) extends Error {
    override def message = "Duplicate %s '%s'".format(what, name)
  }

  /* Context analysis: generic error with location hint */
  case class AttributedError(what: String, where: Attribute) extends Error {
    override def message = what
  }


  /**
    * Wrapper for convenient access to an error message and its location in pattern matching. 
    */
  object LocatedError {
    def unapply(e : Error) : Option[(String, Location)] = e match {
      case UndefinedError(_, _, AttributeImpl(l)) => Some(e.message, l)
      case TypeError(_, AttributeImpl(l), _)      => Some(e.message, l)
      case AttributedError(_, AttributeImpl(l))   => Some(e.message, l)
      case _                                      => None
    }
  }

  /* Context analysis: unimplemented function declaration */
  case class NotImplementedError(what: String, name: String, where: Attribute) extends Error

  def errorMessage(e: Error): String = e match {
    case GenericError        (what)               => "Error: " + what + "\n"
    case CouldNotRun         (what, why)          => "Could not run " + what + ", for the following reason:\n" + why.toString + "\n"
    case NotYetImplemented                        => "Error: Unimplemented feature"
    case InvalidPathError    (what, where)        => where.toString + ": invalid path " + quote(what) + "\n"
    case DuplicatePathError  (what, where)        => where.toString + ": duplicate path " + quote(what) + "\n"
    case DuplicateModuleError(what, where)        => where.toString + ": duplicate module name " + quote(what) + "\n"
    case ParseError          (what, where)        => where.toString + ": " + what + "\n"
    case CircularDependencyError(what) 			  => what + "\n"
    case FileNotFoundError	 (file) 			  => "File not found:" + file + "\n"
    case FilesNotFoundError  (details, f1, f2)	  => details + "Neither of the files exists:" + f1 +", " + f2 + "\n"
    case UndefinedError      (what, name, where)  => where.toString + ": undefined " + what + ": " + quote(name) + "\n"
    case TypeError           (what, where, cause) => where.toString + ": " + what + ", for the following reason:\n" + cause.toString  + "\n"
    case DuplicateError      (what, name, where)  => "Duplicate definition of " + quote(name) + ": " + what + ". Locations:\n" + where.map(_.toString).mkString("\n") + "\n"
    case AttributedError     (what, where)        => where.toString + ": " + what + "\n"
    case NotImplementedError (what, name, where)  => where.toString + ": " + quote(name) + " declared but not implemented. " + what + "\n"
    case ErrorList           (errors)             => errors.map(_.toString).mkString("")
    case _ => "Unknown error\n"
  }

  /**
    * Apply a function which might result in an Error to all elements of a list
    * and combine the results accordingly, i.e., all occurring errors are collected
    * and stored in an ErrorList.
    */
  def errorMap[S,T](ts: List[T], f: T => Either[Error, S]): Either[Error, List[S]] = {

    def combine[T](e1: Either[Error, T], e2: Either[Error, List[T]]) = e1 match {
      case Left(error1) => e2 match {
        case Left(ErrorList(errors)) => Left(ErrorList(error1 :: errors))
        case Left(error2)            => Left(error1 <+> error2)
        case _                       => Left(error1)
      }
      case Right(s) => e2 match {
        case Right(t)    => Right(s :: t)
        case Left(error) => Left(error)
      }
    }

    val z: Either[Error, List[S]] = Right(Nil)
    (ts.map(f) :\ z)(combine)
  }

  /**
    * Collect errors over two computations.
    */
  def collectErrors(e1: Either[Error, Unit], e2: Either[Error, Unit]) = e1 match {
    case Left(error1) => e2 match {
      case Left(error2) => Left(error1 <+> error2)
      case Right(_)     => Left(error1) // changed by Rico 7/24/2013 before "Right()"
    }

    case Right(_) => e2 match {
      case Left(error) => Left(error)
      case Right(_)    => Right()
    }
  }

  
  /**
    * Put a string in `quotes'. Useful for producing pretty error messages.
    */
  def quote(str: String): String = "`" + str + "'"

  /**
    * Quote a list of strings.
    */
  def quote(list: List[String]): String = list.map(quote).mkString(", ")

  /**
    * Quote a set of strings.
    */
  def quote(set: Set[String]): String = quote(set.toList)
}
