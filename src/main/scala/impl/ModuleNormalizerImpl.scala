package de.tuberlin.uebb.sl2.impl

import de.tuberlin.uebb.sl2.modules._

trait ModuleNormalizerImpl extends ModuleNormalizer {
  this: Syntax with SyntaxTraversal with Type with ModuleResolverImpl =>

  def qualifyUnqualifiedModules(program: Program, imports: List[ResolvedImport]) : Program = {
    val declarations = getImportedUnqualifiedDeclarations(imports).filterKeys{
      // allow defined functions to be overridden.
      name => !program.signatures.contains(name)
    }
    val f : PartialFunction[Any, Any] = {
      case Syntax.Var(ide, "") => {Syntax.Var(ide, declarations.getOrElse(ide, Syntax.LocalMod))}
      case Syntax.ConVar(ide, "") => {Syntax.ConVar(ide, declarations.getOrElse(ide, Syntax.LocalMod))}
      case tv: Syntax.TConVar => {tv}
    }
    map(f, program)
  }
  
  def getImportedUnqualifiedDeclarations(imports: List[ResolvedImport]) = {
    imports.filter(_.isInstanceOf[ResolvedUnqualifiedImport]).flatMap(imp =>
      { val rimp = imp.asInstanceOf[ResolvedUnqualifiedImport]
        rimp.signature.dataDefs.map(_.constructors).flatten.map( _.constructor -> rimp.name) ++
        rimp.signature.signatures.keySet.map(k => k -> rimp.name) } ).toMap
  }
  
  /**
   * Normalizes the signature of imported modules.
   * It will substitute the module name of ConVar and TConVar identifiers.
   */
  def normalizeModules(imports : List[ResolvedImport]) : List[ResolvedImport] = {
    // initial path module map with known modules
    // this will be extended with unknown modules
    var pathMod = buildPathModuleMap(imports)
    // current number of unknown modules
    // this value will be used for generating module names
    var unknownId = 0
    
    // collect unknown modules
    // normalize modules
    imports.map(imp => {
      val modPath = buildModulePathMap(imp)
      
      // get the set of unknown modules identified by their paths
      val paths = modPath.toSet.map((kv : (ModuleVar, String)) => kv._2)
      val unknowns = paths &~ pathMod.keySet
      
      // give the unknowns a common name
      // add them to the path module map
      for (u <- unknowns) {
        unknownId += 1
        pathMod += (u -> ("#" + unknownId))
      }
      
      // generate module substitution function
      val sub = normalizeModuleVar(pathMod, modPath)
      // normalize the current module
      normalizeModule(sub)(imp)
    })
  }
  
  // Some List and Map mappers
  
  private def normalizeImports(sub : ModuleVar => ModuleVar) : List[Import]               => List[Import]               = _.map(normalizeImport(sub))
  private def normalizeSigs   (sub : ModuleVar => ModuleVar) : Map [VarName, FunctionSig] => Map [VarName, FunctionSig] = _.mapValues(normalizeSig(sub))
  private def normalizeTypes  (sub : ModuleVar => ModuleVar) : List[ASTType]              => List[ASTType]              = _.map(normalizeType(sub))
  private def normalizeDatas  (sub : ModuleVar => ModuleVar) : List[DataDef]              => List[DataDef]              = _.map(normalizeData(sub))
  private def normalizeCtors  (sub : ModuleVar => ModuleVar) : List[ConstructorDef]       => List[ConstructorDef]       = _.map(normalizeCtor(sub))
  
  // actual substitution methods
  
  // note: this could have been a lot easier if the map function from SyntaxTraversale would have been utilized
  
  private def normalizeModule(sub : ModuleVar => ModuleVar) : ResolvedImport => ResolvedImport = imp => imp match {
    // normalize only qualified imports
    case ui: ResolvedUnqualifiedImport => ui
    case ResolvedQualifiedImport(name, path, file, jsFile, Program(imports, sigs, defs, exts, datas, attrs), ast) =>
      val normImports = normalizeImports(sub)(imports)
      val normSigs    = normalizeSigs   (sub)(sigs   )
      val normDatas   = normalizeDatas  (sub)(datas  )
      val normProg    = Program(normImports, normSigs, defs, exts, normDatas, attrs)
      
      ResolvedQualifiedImport(name, path, file, jsFile, normProg, ast)
    case ei : ResolvedExternImport => ei
  }
  
  private def normalizeImport(sub : ModuleVar => ModuleVar) : Import => Import = imp => imp match {
    // normalize only qualified imports
    case ui : UnqualifiedImport => ui
    case QualifiedImport(path, name, attr) => QualifiedImport(path, sub(name), attr)
    case ei : ExternImport => ei
  }
  
  private def normalizeSig(sub : ModuleVar => ModuleVar) : FunctionSig => FunctionSig = sig => {
    val normType = normalizeType(sub)(sig.typ)
    
    FunctionSig(normType, sig.modifier, sig.attribute)
  }
  
  private def normalizeType(sub : ModuleVar => ModuleVar) : ASTType => ASTType = typ => typ match {
    case tv : TyVar => tv
    case FunTy(types, attr) =>
      val normTypes = normalizeTypes(sub)(types)
      
      FunTy(normTypes, attr)
    case TyExpr(conType, typeParams, attr) =>
      val normConType    = normalizeConType(sub)(conType   )
      val normTypeParams = normalizeTypes  (sub)(typeParams)
      
      TyExpr(normConType, normTypeParams, attr)
  }

  private def normalizeData(sub : ModuleVar => ModuleVar) : DataDef => DataDef = data => {
    val normCtors = normalizeCtors(sub)(data.constructors)
    
    DataDef(data.ide, data.tvars, normCtors, data.modifier, data.attribute)
  }
  
  private def normalizeCtor(sub : ModuleVar => ModuleVar) : ConstructorDef => ConstructorDef = ctor => {
    val normTypes = normalizeTypes(sub)(ctor.types)
    
    ConstructorDef(ctor.constructor, normTypes, ctor.attribute)
  }
  
  private def normalizeModuleVar(pathMod : Map[String, ModuleVar], modPath : Map[ModuleVar, String]) : ModuleVar => ModuleVar = ide => {
    val path = modPath.get(ide).get
    val normMod = pathMod.get(path).get
    
    normMod
  }
  
  private def normalizeConType(sub : ModuleVar => ModuleVar) : TConVar => TConVar = {
    tcv => 
      if (BaseType.typeVars.contains(tcv)) {
        tcv
      } else {
        Syntax.TConVar(tcv.ide, sub(tcv.module))
      }
  }
  
  /**
   * Builds a map from indirect imports (imports of imports) to translate
   * their module names to unique paths.
   */
  private def buildPathModuleMap(imports : List[ResolvedImport]) : Map[String, ModuleVar] = {
    // build map only from qualified imports
    val unqualifiedPaths = imports.filter(_.isInstanceOf[ResolvedUnqualifiedImport]).map(imp => {
      val uqi = imp.asInstanceOf[ResolvedUnqualifiedImport]
      (uqi.path -> Syntax.LocalMod)
    }).toMap
    val qualifiedPaths = imports.filter(_.isInstanceOf[ResolvedQualifiedImport]).map(imp => {
      val rqi = imp.asInstanceOf[ResolvedQualifiedImport]
      
      // use path as key
      // use name as value
      (rqi.path -> rqi.name)
    }).toMap
    // merge the two maps
    //TODO: was macht das? hier stand eben noch unqualifiedPaths.getOrElse(k,0), woduch selbst
    // die typen von konstruktoren und funktionen, die direkt aus demselben modul kamen clashten
    // -- ben 2013/07/13
    unqualifiedPaths ++ qualifiedPaths.map{ case (k,v) => k -> (v + unqualifiedPaths.getOrElse(k,"")) }
  }
  
  /**
   * Builds a map from the local imports to translate
   * unique paths to the local module name.
   */
  private def buildModulePathMap(rimp : ResolvedImport) : Map[ModuleVar, String] = rimp match {
    // build map only from qualified imports
    case rui : ResolvedUnqualifiedImport =>
      val imports = rui.signature.imports
      
      // map the imports of this imported module
      imports.filter(_.isInstanceOf[QualifiedImport]).map(imp => {
        val qi = imp.asInstanceOf[QualifiedImport]
        (qi.name -> qi.path)
      }).toMap +
      // add a translation for local modules
      (Syntax.LocalMod -> rui.path)
      
    case rqi : ResolvedQualifiedImport =>
      val imports = rqi.signature.imports
      
      // map the imports of this imported module
      imports.filter(_.isInstanceOf[QualifiedImport]).map(imp => {
        val qi = imp.asInstanceOf[QualifiedImport]
        (qi.name -> qi.path)
      }).toMap +
      // add a translation for local modules
      (Syntax.LocalMod -> rqi.path)
      
    case _ : ResolvedExternImport => Map()
  }

}
