package de.tuberlin.uebb.sl2.tests.impl

import de.tuberlin.uebb.sl2.modules._
import de.tuberlin.uebb.sl2.impl._
import de.tuberlin.uebb.sl2.tests.specs.ModuleResolverSpec

class ModuleResolverTest
  extends ModuleResolverSpec
  with AbstractFile
  with ModuleResolverImpl
  with Syntax
  with Errors
  with Configs
  with SignatureJsonSerializer
{
  
  def testedImplementationName = "Module resolver"
  
}