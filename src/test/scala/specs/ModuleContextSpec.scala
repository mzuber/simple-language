package de.tuberlin.uebb.sl2.tests.specs

import org.scalatest.matchers._
import org.scalatest.FunSpec
import de.tuberlin.uebb.sl2.modules._

trait ModuleContextSpec extends FunSpec with ShouldMatchers {
  this : ModuleContext
  with ModuleResolver
  with ModuleNormalizer
  with Syntax
  with SyntaxTraversal
  with Context
  with Parser =>
  
  def testedImplementationName(): String

  implicit class SignatureString(str: String) {
    def parsed = {
      parseAst(str).right.get
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

  describe(testedImplementationName() + " this is just debugging println") {
    it("Should be debugged by programmer") {
      val astA = """
        DATA DataA = ConsA
        
        FUN funA : DataA
      """.parsed.asInstanceOf[Program]
      
      val astB1 = """
        IMPORT "a" AS A1
        DATA DataB1 = ConsB1a A1.DataA | ConsB1b
        FUN funB1a : A1.DataA
        FUN funB1b : DataB1 -> A1.DataA
      """.parsed.asInstanceOf[Program]
      
      val astB2 = """
        IMPORT "a" AS A2
        DATA DataB2 = ConsB2a A2.DataA | ConsB2b
        FUN funB2a : A2.DataA -> DataB2
        FUN funB2b : DataB2
      """.parsed.asInstanceOf[Program]
      
      val astC = """
        IMPORT "b1" AS B1
        IMPORT "b2" AS B2
        
        FUN funC : B1.DataB1 -> B2.DataB2
        DEF funC x = b2funB2a(B1.funB1b(x))
      """.parsed.asInstanceOf[Program]
      
      val importCB1 = ResolvedQualifiedImport("B1", "b1", null, astB1, astC.getImport("B1"))
      val importCB2 = ResolvedQualifiedImport("B2", "b2", null, astB2, astC.getImport("B2"))
      
      val astX = """
        IMPORT "a" AS A
        IMPORT "b1" AS B1
        IMPORT "b2" AS B2
        
        FUN funX : A.DataA -> B2.DataB2
        DEF funX = B2.funB2a
      """.parsed.asInstanceOf[Program]
      
      val importXA  = ResolvedQualifiedImport("A" , "a" , null, astA , astX.getImport("A" ))
      val importXB1 = ResolvedQualifiedImport("B1", "b1", null, astB1, astX.getImport("B1"))
      val importXB2 = ResolvedQualifiedImport("B2", "b2", null, astB2, astX.getImport("B2"))
      
      val importsC = normalizeModules(List(importCB1, importCB2))
      val importsX = normalizeModules(List(importXA, importXB1, importXB2))
      
      val contextC = buildModuleContext(importsC)
      val contextX = buildModuleContext(importsX)
      
//      println(normalizeModules(List(importCB1, importCB2)))
//      println(normalizeModules(List(importXA, importXB1, importXB2)))

//      println(contextC)
//      println(contextX)
    }
  }
  
}
