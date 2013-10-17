package de.tuberlin.uebb.sl2.impl

import scala.util.parsing.json._
import de.tuberlin.uebb.sl2.modules.{ SignatureSerializer, Syntax, Errors }

trait SignatureJsonSerializer extends SignatureSerializer with Syntax with Errors {

  def serialize(ast : AST) : String = ast2Json(ast).toString
  
  def deserialize(jsonString : String, location : Location = NoLocation) : AST = {
    JSON.parseFull(jsonString) match {
      case Some(result) =>
        val jsonAst = result.asInstanceOf[JsonImportAst]
        json2Ast(jsonAst, location)
      case None => null
    }
  }
  
  // syntax' json export types
  type JsonExportAst                = JSONObject
  type JsonExportAstType            = Any
  type JsonExportAstTypeList        = JSONArray

  type JsonExportImport             = JSONObject
  type JsonExportImportList         = JSONArray
  type JsonExportPath               = String
  type JsonExportFunctionSig        = JsonExportAstType
  type JsonExportFunctionSigMap     = JSONObject
  type JsonExportDataDef            = JSONObject
  type JsonExportDataDefList        = JSONArray
  type JsonExportConstructorDef     = JSONObject
  type JsonExportConstructorDefList = JSONArray
  
  type JsonExportTyVar              = JsonExportTypeVarName
  type JsonExportFunTy              = JsonExportAstTypeList
  type JsonExportTyExpr             = JSONObject  
  
  // syntax' json import types
  type JsonImportAst                = Map[String, Any]
  type JsonImportAstType            = Any
  type JsonImportAstTypeList        = List[JsonImportAstType]
  
  type JsonImportImport             = Map[String, Any]
  type JsonImportImportList         = List[JsonImportImport]
  type JsonImportPath               = String
  type JsonImportFunctionSig        = JsonImportAstType
  type JsonImportFunctionSigMap     = Map[String, JsonImportFunctionSig]
  type JsonImportDataDef            = Map[String, Any]
  type JsonImportDataDefList        = List[JsonImportDataDef]
  type JsonImportConstructorDef     = Map[String, Any]
  type JsonImportConstructorDefList = List[JsonImportConstructorDef]
  
  type JsonImportTyVar              = JsonImportTypeVarName
  type JsonImportFunTy              = JsonImportAstTypeList
  type JsonImportTyExpr             = Map[String, Any]
  
  // identifier's json export types
  type JsonExportModuleVar = String
//  type JsonExportVar       = String
//  type JsonExportConVar    = String
  type JsonExportTConVar   = String
  
  type JsonExportVarName         = String
  type JsonExportTypeVarName     = String
  type JsonExportTypeVarNameList = JSONArray
  type JsonExportConVarName      = String
  type JsonExportTConVarName     = String  
  
  // identifier's json import types
  type JsonImportModuleVar = String
//  type JsonImportVar       = String
//  type JsonImportConVar    = String
  type JsonImportTConVar   = String

  type JsonImportVarName         = String
  type JsonImportTypeVarName     = String
  type JsonImportTypeVarNameList = List[JsonImportTypeVarName]
  type JsonImportConVarName      = String
  type JsonImportTConVarName     = String
  
  // identifier conversion to json
  private def moduleVar2Json(v : ModuleVar) : JsonExportModuleVar = v
  private def      path2Json(v : String   ) : JsonExportPath      = v
//  private def       var2Json(v : Var      ) : JsonExportVar       = v.module + "." + v.ide
//  private def    conVar2Json(v : ConVar   ) : JsonExportConVar    = v.module + "." + v.ide
  private def   tConVar2Json(v : TConVar  ) : JsonExportTConVar   = v.module + "." + v.ide
  
  private def     varName2Json(v : VarName    ) : JsonExportVarName     = v
  private def typeVarName2Json(v : TypeVarName) : JsonExportTypeVarName = v
  private def  conVarName2Json(v : ConVarName ) : JsonExportConVarName  = v
  private def tConVarName2Json(v : TConVarName) : JsonExportTConVarName = v
  
  // identifier conversion from json  
  private def json2ModuleVar(json : JsonImportModuleVar) : ModuleVar = json
  private def json2Path     (json : JsonImportPath     ) : String    = json
//  private def json2Var      (json : JsonImportVar      ) : Var       = { val (module, ide) = splitQualified(json); Syntax.Var(ide, module) }
//  private def json2ConVar   (json : JsonImportConVar   ) : ConVar    = { val (module, ide) = splitQualified(json); Syntax.ConVar(ide, module) }
  private def json2TConVar  (json : JsonImportTConVar  ) : TConVar   = { val (module, ide) = splitQualified(json); Syntax.TConVar(ide, module) }
  
  private def json2VarName    (json : JsonImportVarName    ) : VarName     = json
  private def json2TypeVarName(json : JsonImportTypeVarName) : TypeVarName = json
  private def json2ConVarName (json : JsonImportConVarName ) : ConVarName  = json
  private def json2TConVarName(json : JsonImportTypeVarName) : TConVarName = json

  
  private def splitQualified(json : String) : (String, String) = {
    val chunks = json.split('.')
    
    (chunks(0), chunks(1))
  }

  // some map macros
  
  private def typeVarNames2Json(ides : List[TypeVarName]) : JsonExportTypeVarNameList = JSONArray(ides.map(typeVarName2Json))
  
  private def json2TypeVarNames(jsonIdes : JsonImportTypeVarNameList) : List[TypeVarName] = jsonIdes.map(json2TypeVarName)
  
  private def imports2Json(imports : List[Import]             ) : JsonExportImportList         = JSONArray (imports.map(import2Json))
  private def    sigs2Json(sigs    : Map[VarName, FunctionSig]) : JsonExportFunctionSigMap     = JSONObject(sigs   .map(   sig2Json))
  private def   types2Json(types   : List[ASTType]            ) : JsonExportAstTypeList        = JSONArray (types  .map(  type2Json))
  private def   datas2Json(datas   : List[DataDef]            ) : JsonExportDataDefList        = JSONArray (datas  .map(  data2Json))
  private def   ctors2Json(ctors   : List[ConstructorDef]     ) : JsonExportConstructorDefList = JSONArray (ctors  .map(  ctor2Json))

  private def json2Imports(jsonImports : JsonImportImportList                             ) : List[Import]              = jsonImports.map(json2Import           )
  private def json2Sigs   (jsonSigs    : JsonImportFunctionSigMap    , location : Location) : Map[VarName, FunctionSig] = jsonSigs   .map(json2Sig (_, location))
  private def json2Types  (jsonTypes   : JsonImportAstTypeList                            ) : List[ASTType]             = jsonTypes  .map(json2Type             )
  private def json2Datas  (jsonDatas   : JsonImportDataDefList       , location : Location) : List[DataDef]             = jsonDatas  .map(json2Data(_, location))
  private def json2Ctors  (jsonCtors   : JsonImportConstructorDefList, location : Location) : List[ConstructorDef]      = jsonCtors  .map(json2Ctor(_, location))

  // actual (de)serialization implementation starts here
  
  private def ast2Json(ast : AST) : JsonExportAst = ast match { case Program(imports, sigs, _, _, datas, _) =>
    val exportedSigs = sigs.filter{
        case (_, FunctionSig(_,modi,_)) => PublicModifier == modi}
    val root : Map[String, Any] = Map(
      "imports"    -> imports2Json(imports     ),
      "signatures" ->    sigs2Json(exportedSigs),
      "dataDefs"   ->   datas2Json(datas       )
    )
    
    JSONObject(root)
  }
  
  private def json2Ast(jsonAst : JsonImportAst, location : Location) : AST = {
    val jsonImports = jsonAst.get("imports"   ).get.asInstanceOf[JsonImportImportList    ]
    val jsonSigs    = jsonAst.get("signatures").get.asInstanceOf[JsonImportFunctionSigMap]
    val jsonDatas   = jsonAst.get("dataDefs"  ).get.asInstanceOf[JsonImportDataDefList   ]
    
    Program(json2Imports(jsonImports), json2Sigs(jsonSigs, location), Map(), Map(), json2Datas(jsonDatas, location))
  }
  
  private def import2Json(_import : Import) : JsonExportImport = _import match {
    case QualifiedImport(path, name, _) =>
    JSONObject(Map[String, Any](
        ("name" -> moduleVar2Json(name)),
        ("path" ->      path2Json(path))))
    case ExternImport(path, _) =>
      JSONObject(Map[String, Any](
        ("name" -> moduleVar2Json("_EXTERN_")),
        ("path" ->      path2Json(path))))
  }
  
  private def json2Import(jsonImport : JsonImportImport) : Import = {
    val jsonName = jsonImport.get("name").get.asInstanceOf[JsonImportModuleVar]
    val jsonPath = jsonImport.get("path").get.asInstanceOf[JsonImportPath     ]
    
    QualifiedImport(json2Path(jsonPath), json2ModuleVar(jsonName))
  }

  private def sig2Json(sig : (VarName, FunctionSig)) : (JsonExportVarName, JsonExportFunctionSig) = {
    val (ide, funSig) = sig
    
    (varName2Json(ide), type2Json(funSig.typ))
  }
    
  private def json2Sig(jsonSig : (JsonImportVarName, JsonImportFunctionSig), location : Location) : (VarName, FunctionSig) = {
    val (fun, sig) = jsonSig
    
    (json2VarName(fun), FunctionSig(json2Type(sig), DefaultModifier, AttributeImpl(location)))
  }

  private def type2Json(typ : ASTType) : JsonExportAstType = typ match {
    case TyVar(ide, _) => typeVarName2Json(ide)
    case TyExpr(con, params, _) =>
      val map : Map[String, Any] = Map(
        "type"   -> tConVar2Json(con   ),
        "params" -> types2Json  (params)
      )
      
      JSONObject(map)
    case FunTy(types, _) => types2Json(types)
  }
    
  private def json2Type(jsonType : JsonImportAstType) : ASTType = jsonType match {
    case jsonTyVar  : JsonImportTyVar  => TyVar(json2TypeVarName(jsonTyVar))
    case jsonTyExpr : JsonImportTyExpr =>
      val con    = jsonTyExpr.get("type"  ).get.asInstanceOf[JsonImportTConVar    ]
      val params = jsonTyExpr.get("params").get.asInstanceOf[JsonImportAstTypeList]
      
      TyExpr(json2TConVar(con), json2Types(params))
    case jsonFunTy : JsonImportFunTy => FunTy(json2Types(jsonFunTy))
  }

  private def data2Json(data : DataDef) : JsonExportDataDef = {
    val map : Map[String, Any] = Map(
      "ide"          -> tConVarName2Json (data.ide         ),
      "tvars"        -> typeVarNames2Json(data.tvars       ),
      "constructors" -> {
        // the constructors of private types are not exported.
        if (data.modifier == PublicModifier)
          ctors2Json(data.constructors)
        else
          JSONArray(List())
      }
    )
    
    JSONObject(map)
  }
    
  private def json2Data(jsonData : JsonImportDataDef, location : Location) : DataDef = {
    val jsonIde   = jsonData.get("ide"         ).get.asInstanceOf[JsonImportTConVarName       ]
    val jsonTvars = jsonData.get("tvars"       ).get.asInstanceOf[JsonImportTypeVarNameList   ]
    val jsonCtors = jsonData.get("constructors").get.asInstanceOf[JsonImportConstructorDefList]
    
    DataDef(
      json2TConVarName (jsonIde            ),
      json2TypeVarNames(jsonTvars          ),
      json2Ctors       (jsonCtors, location),
      DefaultModifier,
      AttributeImpl(location)
    )
  }

  private def ctor2Json(ctor : ConstructorDef) : JsonExportConstructorDef = {
    val map : Map[String, Any] = Map(
      "constructor" -> conVarName2Json(ctor.constructor),
      "types"       -> types2Json     (ctor.types      )
    )
    
    JSONObject(map)
  }
  
  private def json2Ctor(jsonCtor : JsonImportConstructorDef, location : Location) : ConstructorDef = {
    val jsonIde   = jsonCtor.get("constructor").get.asInstanceOf[JsonImportConVarName ]
    val jsonTypes = jsonCtor.get("types"      ).get.asInstanceOf[JsonImportAstTypeList]
    
    ConstructorDef(json2ConVarName(jsonIde), json2Types(jsonTypes), AttributeImpl(location))
  }

}
