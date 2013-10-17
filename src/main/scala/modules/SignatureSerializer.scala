package de.tuberlin.uebb.sl2.modules

trait SignatureSerializer {
  this : Syntax =>

  def serialize(in : AST) : String
  
  def deserialize(in : String, location : Location = NoLocation) : AST

}