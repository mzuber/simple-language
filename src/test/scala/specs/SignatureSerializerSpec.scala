package de.tuberlin.uebb.sl2.tests.specs

import org.scalatest.matchers._
import org.scalatest.FunSpec
import scala.language.implicitConversions
import de.tuberlin.uebb.sl2.modules._
import de.tuberlin.uebb.sl2.impl.ParboiledParser

trait SignatureSerializerSpec extends FunSpec with ShouldMatchers with ParboiledParser {
  this : SignatureSerializer with Syntax with Errors =>

  def testedImplementationName(): String
  
  case class ParseSerializeDeserializeResult(parsed : AST, serialized : String, deserialized : AST)
  
  implicit class ParsingInput(s: String) {
    def parsedSerializedAndDeserialized() = {
      val ast = parseAst(s).right.get
      val serialized = serialize(ast)
      val deserialized = deserialize(serialized)
      
      ParseSerializeDeserializeResult(ast, serialized, deserialized)
    }
  }
      
  implicit class SignatureString(str: String) {
    def parsed = {
      parseAst(str).right.get
    }
    
    def parsedAndSerialized() = {
      serialize(str.parsed)
    }
  }

  def equalParsed = IndirectSignatureMatcher
  def equal(expected : AST) = SignatureMatcher(expected)

  case class IndirectSignatureMatcher() extends Matcher[ParseSerializeDeserializeResult] {
    def apply(delivered : ParseSerializeDeserializeResult) =
      SignatureMatcher(delivered.parsed).apply(delivered.deserialized)
  }
  
  // This matcher basically only matches the strings of the signature's identifiers.
  // Therefore, some equal signatures may be recognized as unequal since type variables can
  // be substituted.
  // Also the order of data definitions shouldn't matter. For the sake of simplicity we
  // assume that the definitions weren't reordered during the serialization and deserialization
  // process.
  // A more proper implementation may normalizes both signatures before matching.
  case class SignatureMatcher(expected : AST) extends Matcher[AST] {
    def apply(delivered : AST) = {
      val result = expected match {
        case Program(parsedImports, parsedSig, _, _, parsedData, _) => delivered match {
          case Program(deserializedImports, deserializedSig, _, _, deserializedData, _) =>
             compareImports(parsedImports, deserializedImports) &&
            compareSigs   (parsedSig    , deserializedSig    ) &&
            compareDatas  (parsedData   , deserializedData   )
          }
      }
      
      MatchResult(result, "the signatures don't match", "the signatures match", "ms failure", "neg ms failure")
    }
    
    def compareList[T](lhs : List[T], compare : (T, T) => Boolean, rhs : List[T]) : Boolean = {
      if (lhs.size != rhs.size)
        return false
      if (lhs.size == 0)
        return true;
      
      // compare each pair of lhs and rhs elements
      // return true if all elements were equal
      (lhs, rhs).zipped.map((l, r) => compare(l, r)).reduce((l, r) => l && r)
    }
    
    def compareMap[S, T](lhs : Map[S, T], compare : (T, T) => Boolean, rhs : Map[S, T]) : Boolean = {
      if (lhs.size != rhs.size)
        return false
      
      // compare with each corresponding element
      for ((key, lvalue) <- lhs) {
        rhs.get(key) match {
          case Some(rvalue) => if (!compare(lvalue, rvalue)) return false
          case None         => return false
        }
      }
    
      true
    }
    
    def compareImports     (lhs : List[Import]              , rhs : List[Import]              ) : Boolean = compareList(lhs, compareImport     , rhs)
    def compareSigs        (lhs : Map [VarName, FunctionSig], rhs : Map [VarName, FunctionSig]) : Boolean = compareMap (lhs, compareSig        , rhs)
    def compareDatas       (lhs : List[DataDef]             , rhs : List[DataDef]             ) : Boolean = compareList(lhs, compareData       , rhs)
    def compareConstructors(lhs : List[ConstructorDef]      , rhs : List[ConstructorDef]      ) : Boolean = compareList(lhs, compareConstructor, rhs)
    def compareAstTypes    (lhs : List[ASTType]             , rhs : List[ASTType]             ) : Boolean = compareList(lhs, compareAstType    , rhs)
    def compareTypeVarNames(lhs : List[TypeVarName]         , rhs : List[TypeVarName]         ) : Boolean = compareList(lhs, compareTypeVarName, rhs)
    
    // identifiers are matched on string level
    // note that this may be insufficient because of equivalent substittions
    def compareModuleVar(lhs : ModuleVar, rhs : ModuleVar) : Boolean = lhs.equals(rhs)
    def comparePath(lhs : String, rhs : String) : Boolean = lhs.equals(rhs)
    def compareTConVar(lhs : TConVar, rhs : TConVar) : Boolean = lhs == rhs

    def compareTypeVarName(lhs : TypeVarName, rhs : TypeVarName) : Boolean = lhs.equals(rhs)
    def compareConVarName (lhs : ConVarName , rhs : ConVarName ) : Boolean = lhs.equals(rhs)
    def compareTConVarName(lhs : TConVarName, rhs : TConVarName) : Boolean = lhs.equals(rhs)
    
    def compareImport(lhs : Import, rhs : Import) : Boolean = (lhs, rhs) match { case (lhs : QualifiedImport, rhs : QualifiedImport) =>
      compareModuleVar(lhs.name, rhs.name) &&
        comparePath(lhs.path, rhs.path)
    }

    def compareSig(lhs : FunctionSig, rhs : FunctionSig) : Boolean = compareAstType(lhs.typ, rhs.typ)
    
    def compareAstType(lhs : ASTType, rhs : ASTType) : Boolean = {
      (lhs, rhs) match {
        case (lhs : TyVar , rhs : TyVar ) => compareTypeVarName(lhs.ide, rhs.ide)
        case (lhs : FunTy , rhs : FunTy ) => compareAstTypes(lhs.types, rhs.types)
        case (lhs : TyExpr, rhs : TyExpr) => 
          compareTConVar(lhs.conType, rhs.conType) &&
            compareAstTypes(lhs.typeParams, rhs.typeParams)
        case _ => false
      }
    }
    
    def compareData(lhs : DataDef, rhs : DataDef) : Boolean =
      compareTConVarName(lhs.ide, rhs.ide) &&
        compareTypeVarNames(lhs.tvars, rhs.tvars) &&
        compareConstructors(lhs.constructors, rhs.constructors)
    
    def compareConstructor(lhs : ConstructorDef, rhs : ConstructorDef) : Boolean = 
      compareConVarName(lhs.constructor, rhs.constructor) &&
        compareAstTypes(lhs.types, rhs.types)

  }
  
  describe(testedImplementationName() + " signature matcher test (yes we are testing the testers!)") {
    it("Should fail on unequal import paths") {
      "IMPORT \"path\" AS M".parsed should not equal("IMPORT \"elsewhere\" AS M".parsed)
    }
    it("Should succeed on equal import names") {
      "IMPORT \"path\" AS M".parsed should equal("IMPORT \"path\" AS M".parsed)
    }
    it("Should fail on unequal function identifiers") {
      "FUN f : (X a) -> y".parsed should not equal("FUN g : (X a) -> y".parsed)
    }
    it("Should fail on unequal type lists") {
      "FUN f : (X a) -> y".parsed should not equal("FUN g : (X a) -> y -> z".parsed)
    }
    it("Should fail on unequal type identifiers") {
      "FUN f : (X a) -> y".parsed should not equal("FUN f : (X b) -> z".parsed)
    }
    it("Should fail on unequal type variables") {
      "FUN f : (X a) -> y".parsed should not equal("FUN f : (X b) -> y".parsed)
    }
    it("Should fail on unequal data identifiers in functions") {
      "FUN f : (X a) -> y".parsed should not equal("FUN f : (Y b) -> y".parsed)
    }
    it("Should fail on unequal module identifiers") {
      "FUN f : M.X".parsed should not equal("FUN f : N.X".parsed)
    }
    it("Should fail on unequal data identifiers in DATA definitions") {
      "DATA Type a b c = Cons1 a b | Cons2 c".parsed should not equal("DATA Epyt a b c = Cons1 a b | Cons2 c".parsed)
    }
    it("Should fail on unequal constructor identifiers") {
      "DATA Type a b c = Cons1 a b | Cons2 c".parsed should not equal("DATA Type a b c = Cons1 a b | Cons3 c".parsed)
    }
    it("Should fail on unequal constructor lists") {
      "DATA Type a b c = Cons1 a b | Cons2 c".parsed should not equal("DATA Type a b c = Cons1 a b | Cons2 c | Cons3".parsed)
    }
  }

  describe(testedImplementationName() + " serialize and deserialize") {
    it("Should serialize and deserialize one import") {
      "IMPORT \"path\" AS M".parsedSerializedAndDeserialized should equalParsed()
    }
    it("Should serialize and deserialize one function signature") {
      "PUBLIC FUN f : (X a) -> y".parsedSerializedAndDeserialized should equalParsed()
    }
    it("Should serialize and deserialize imported types") {
      "PUBLIC FUN f : M.X -> N.Y".parsedSerializedAndDeserialized should equalParsed()
    }
    it("Should not serialize private definitions.") {
      "FUN f : M.X -> N.Y".parsedSerializedAndDeserialized.deserialized should equal("".parsed)
    }
    it("Should serialize and deserialize one public data definition") {
      "PUBLIC DATA Type a b c = Cons1 a b | Cons2 c".parsedSerializedAndDeserialized should equalParsed()
    }
    it("Should not serialize private data constructors") {
      "DATA Type a = Cons a".parsedSerializedAndDeserialized.deserialized should equal(Program(List(), Map(), Map(), Map(), List(DataDef("Type", List("a"), List()))))
    }
  }
  
}
