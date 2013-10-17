package de.tuberlin.uebb.sl2.tests.specs

import org.scalatest.matchers._
import org.scalatest.FunSpec
import de.tuberlin.uebb.sl2.modules._

trait ModuleNormalizerSpec extends FunSpec with ShouldMatchers {
  this : ModuleNormalizer
  with Syntax
  with SyntaxTraversal
  with Parser
  with Type
  with Errors
  with ModuleResolver =>
  
  def testedImplementationName(): String
      
  implicit class SignatureString(str: String) {
    def parsed = {
      parseAst(str).right.get.asInstanceOf[Program]
    }
  }
  
  implicit class AstImport(ast : AST) {
    def getImport(name : ModuleVar) : QualifiedImport = ast match { case p : Program =>
      p.imports.filter(imp => imp.isInstanceOf[QualifiedImport]).map(imp => {
        val qi = imp.asInstanceOf[QualifiedImport]
        
        qi.name -> qi
      }).toMap.get(name).get
    }
  }
  
  // helper class to manage multiple modules
  // simulates the resolution of imports
  private class ModuleSet {
    var modules : Map[String, Program] = Map()
  
    def add(path : String, code : String) : Unit = {
      modules += (path -> code.parsed)
    }
    
    def normalize(path : String) : Map[String, Program] = {
      val module = modules.get(path).get
      
      val namedImports = module.imports.map { imp =>
        val qimp = imp.asInstanceOf[QualifiedImport]
        val resolved = modules.get(qimp.path).get
        val rimp = ResolvedQualifiedImport(qimp.name, qimp.path, null, resolved, qimp)
        
        (qimp.path, rimp)
      }
      
      val (paths, imports) = namedImports.unzip
      
      val normalized = normalizeModules(imports).map(_.asInstanceOf[ResolvedQualifiedImport].signature.asInstanceOf[Program])
      
      paths.zip(normalized).toMap
    }
  }

  describe(testedImplementationName() + ": substitution tests") {
    it("Should substitute foreign data types in function declarations") {
      val modules = new ModuleSet()
      
      modules.add("a", """
        DATA DataA = ConsA
        FUN funA : DataA
      """)
      
      modules.add("b", """
        IMPORT "a" AS A
      """)
      
      modules.normalize("b") should be(Map(
        "a" -> """
          DATA DataA = ConsA
          FUN funA : A.DataA
        """.parsed
      ))
    }
    
    it("Should substitute imported data types of imports in function declarations") {
      val modules = new ModuleSet()
      
      // module A
      modules.add("a", """
        DATA DataA = ConsA
        FUN funA : DataA
      """)
      
      // module B
      modules.add("b", """
        IMPORT "a" AS A1
        
        FUN funB : A1.DataA
      """)
      
      // module C
      modules.add("c", """
        IMPORT "a" AS A2
        IMPORT "b" AS B2
      """)
      
      modules.normalize("c") should be(Map(
        ("a" -> """
          DATA DataA = ConsA
          FUN funA : A2.DataA
        """.parsed),
        ("b" -> """
          IMPORT "a" AS A2
          
          FUN funB : A2.DataA
        """.parsed)
      ))
    }
    
    it("Should substitude imported data types of imports in constructors") {
      val modules = new ModuleSet()
      
      // module A
      modules.add("a", """
        DATA DataA = ConsA
        FUN funA : DataA
      """)
      
      // module B
      modules.add("b", """
        IMPORT "a" AS A1
        
        DATA DataB = ConsB A1.DataA
      """)
      
      // module C
      modules.add("c", """
        IMPORT "a" AS A2
        IMPORT "b" AS B2
      """)
      
      modules.normalize("c") should be(Map(
        ("a" -> """
          DATA DataA = ConsA
          FUN funA : A2.DataA
        """.parsed),
        ("b" -> """
          IMPORT "a" AS A2
          
          DATA DataB = ConsB A2.DataA
        """.parsed)
      ))
    }
    
    it("Should substitude unknown data types") {
      val modules = new ModuleSet()
      
      // module A
      modules.add("a", """
        DATA DataA = ConsA
        FUN funA : DataA
      """)
      
      // module B
      modules.add("b", """
        IMPORT "a" AS A1
        
        DATA DataB = ConsB A1.DataA
      """)
      
      // module C
      modules.add("c", """
        IMPORT "b" AS B2
      """)
      
      modules.normalize("c") should be(Map(
        ("b" -> Program(
          List(QualifiedImport("a", "#1")),
          Map(),
          Map(),
          Map(),
          List(
            DataDef("DataB", List(), List(
              ConstructorDef("ConsB", 
                List(
                  TyExpr(Syntax.TConVar("DataA", "#1"), List())
                )
              )
            ))
          )
        ))
      ))
    }
  }
  
}
