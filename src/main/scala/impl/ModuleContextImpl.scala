package de.tuberlin.uebb.sl2.impl

import de.tuberlin.uebb.sl2.modules._

trait ModuleContextImpl extends ModuleContext {
  this : ModuleResolver with Syntax with Context with Type with DTChecker =>
  
  def buildModuleContext(imp : List[ResolvedImport]) : (Context, Map[Var, FunctionSig]) = {
    if (imp.isEmpty) return (Map(), Map())
    
    val contexts = imp.map(buildContext)
    val sigs     = imp.map(buildSig)
    
    val context = contexts.reduce(_ <++> _)
    val sig     = sigs    .reduce(_  ++  _)
    
    (context, sig)
  }
  
  def buildContext(imp : ResolvedImport) : Context = imp match {
    case ui: ResolvedUnqualifiedImport =>
	  dataConTypes(ui.signature.dataDefs) ++
	  	ui.signature.signatures.map(
          {case (funName, sig) => (Syntax.Var(funName).asInstanceOf[VarFirstClass] ->
          							astToType(sig.typ).generalize(Map()))})
    case ResolvedQualifiedImport(name, _, _, prog, _) =>
      dataConTypes(prog.dataDefs, name) ++
        prog.signatures.map(
          {case (funName, sig) => (Syntax.Var(funName, name).asInstanceOf[VarFirstClass] ->
          							astToType(sig.typ).generalize(Map()))})
    case _ : ResolvedExternImport => Map()
  }
    
  def buildSig(imp : ResolvedImport) : Map[Var, FunctionSig] = imp match {
    case ui: ResolvedUnqualifiedImport =>
      ui.signature.signatures.map(kv => {
        val (ide, sig) = kv
        
        (Syntax.Var(ide) -> sig)
      })
    case ResolvedQualifiedImport(name, _, _, prog, _) =>
      prog.signatures.map(kv => {
        val (ide, sig) = kv
        
        (Syntax.Var(ide, name) -> sig)
      })
    case _ : ResolvedExternImport => Map()
  }

}
