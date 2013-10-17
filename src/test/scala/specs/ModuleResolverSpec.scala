package de.tuberlin.uebb.sl2.tests.specs

import org.scalatest.matchers._
import org.scalatest.FunSpec
import de.tuberlin.uebb.sl2.modules._

trait ModuleResolverSpec extends FunSpec with ShouldMatchers {
  this : ModuleResolver with Syntax with Errors =>
  
  def testedImplementationName(): String

  def notFail: Matcher[Either[Error, Unit]] = be(Right())
  
  def fail(err: Error) : Matcher[Either[Error, Unit]] = be(Left(err))

  describe(testedImplementationName() + ": path checking") {
    it("Should refuse empty paths") {
      val imports = List(
        QualifiedImport("", "Empty")
      )
      
      checkImports(imports) should fail(InvalidPathError("", EmptyAttribute))
    }
    
    it("Should refuse capital letter") {
      val imports = List(
        QualifiedImport("DONT/SCREAM", "Capital")
      )
      
      checkImports(imports) should fail(InvalidPathError("DONT/SCREAM", EmptyAttribute))
    }
    
    it("Should refuse dots") {
      val imports = List(
        QualifiedImport("..", "Parent")
      )
      
      checkImports(imports) should fail(InvalidPathError("..", EmptyAttribute))
    }
    
    it("Should refuse directoy") {
      val imports = List(
        QualifiedImport("dir/", "Directory")
      )
      
      checkImports(imports) should fail(InvalidPathError("dir/", EmptyAttribute))
    }
    
    it("Should accept alphabetic path") {
      val imports = List(
        QualifiedImport("my/path", "Ordinary")
      )
      
      checkImports(imports) should notFail
    }
    
    it("Should accept path with underscors ") {
      val imports = List(
        QualifiedImport("my/path/_underscore_", "Underscores")
      )
      
      checkImports(imports) should notFail
    }

    it("Should accept path with numbers ") {
      val imports = List(
        QualifiedImport("my/p4th/num83r5", "Numbers")
      )
      
      checkImports(imports) should notFail
    }
    
    it("Should accept path with dashes") {
      val imports = List(
        QualifiedImport("my/pa-th/-", "Dash")
      )
      
      checkImports(imports) should notFail
    }
    
    it("Should refuse directories with dots") {
      val imports = List(
        QualifiedImport("my/.hidden/path", "Dot")
      )
      
      checkImports(imports) should fail(InvalidPathError("my/.hidden/path", EmptyAttribute))
    }
    
    it("Should refuse dot-only directory") {
      val imports = List(
        QualifiedImport("my/.../path", "Dots")
      )
      
      checkImports(imports) should fail(InvalidPathError("my/.../path", EmptyAttribute))
    }
  }

  describe(testedImplementationName() + ": import uniqueness") {
    it("Should accept unique paths") {
      val imports = List(
        QualifiedImport("my/path", "MyModule"),
        QualifiedImport("other/path", "OtherModule")
      )
      
      checkImports(imports) should notFail
    }
    
    it("Should refuse duplicate paths") {
      val imports = List(
        QualifiedImport("identical/path", "MyModule"),
        QualifiedImport("identical/path", "OtherModule"),
        QualifiedImport("other/path", "InnocentModule")
      )
      
      checkImports(imports) should fail(ErrorList(List(
        DuplicatePathError("identical/path", EmptyAttribute),
        DuplicatePathError("identical/path", EmptyAttribute)
      )))
    }

    it("Should accept unique module identifiers") {
      val imports = List(
        QualifiedImport("my/path", "MyModule"),
        QualifiedImport("other/path", "OtherModule")
      )
      
      checkImports(imports) should notFail
    }
    
    it("Should refuse duplicate module identifiers") {
      val imports = List(
        QualifiedImport("my/path", "Duplicate"),
        QualifiedImport("other/path", "Duplicate"),
        QualifiedImport("innocent", "Innocent")
      )
      
      checkImports(imports) should fail(ErrorList(List(
        DuplicateModuleError("Duplicate", EmptyAttribute),
        DuplicateModuleError("Duplicate", EmptyAttribute)
      )))
    }
  }
  
  describe(testedImplementationName() + ": import resolution") {
    it("Should resolve a qualified import (nyi)") {
      // TODO
    }
    
    it("Should resolve an unqualified import (nyi)") {
      // TODO
    }
    
    it("Should resolve an external import (nyi)") {
      // TODO
    }
    
    // TODO missing files
  }
  
}
