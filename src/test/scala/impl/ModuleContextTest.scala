package de.tuberlin.uebb.sl2.tests.impl

import de.tuberlin.uebb.sl2.modules._
import de.tuberlin.uebb.sl2.impl._
import de.tuberlin.uebb.sl2.tests.specs.ModuleContextSpec

class ModuleContextTest extends
  ModuleContextSpec
  with Configs
  with Errors
  with Syntax
  with SyntaxTraversal
  with Context
  with Type
  with EnrichedLambdaCalculus
  with Substitution
  with Unification
  with NameSupply
  with ParboiledParser
  with AbstractFile
  with ModuleResolverImpl
  with ModuleNormalizerImpl
  with ModuleContextImpl
  with DTCheckerImpl
  with SignatureJsonSerializer
{

  def testedImplementationName = "Substitution module normalizer"

}
