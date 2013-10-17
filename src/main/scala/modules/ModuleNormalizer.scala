package de.tuberlin.uebb.sl2.modules

trait ModuleNormalizer {
  this : Syntax with Type with ModuleResolver =>
  
  def normalizeModules(imports : List[ResolvedImport]) : List[ResolvedImport]
  def qualifyUnqualifiedModules(program: Program, imports: List[ResolvedImport]) : Program

}
