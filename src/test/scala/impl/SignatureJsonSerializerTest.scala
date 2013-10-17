package de.tuberlin.uebb.sl2.tests.impl

import de.tuberlin.uebb.sl2.tests.specs.SignatureSerializerSpec
import de.tuberlin.uebb.sl2.impl.SignatureJsonSerializer

class SignatureJsonSerializerTest extends SignatureSerializerSpec with SignatureJsonSerializer {
  
  def testedImplementationName = "Signature JSON serializer"
  
}
