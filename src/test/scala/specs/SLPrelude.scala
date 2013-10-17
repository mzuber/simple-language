package de.tuberlin.uebb.sl2.specs

import de.tuberlin.uebb.sl2.modules._

/**
 * A handcoded version of identifiers defined in the prelude. (For testing purposes)
 */
trait SLPrelude {
  
  this: Type with Context =>
  
  /*
   * Prelude and built-in functions
   */
  val int = BaseType.Integer
  val real = BaseType.Real
  val char = BaseType.Character
  val bool = BaseType.Bool
  val α = TypeVariable("alpha")
  val β = TypeVariable("beta")
  val list = (t: Type) => TypeConstructor(Syntax.TConVar("List"), List(t))
  val dom = (t: Type) => TypeConstructor(Syntax.TConVar("DOM"), List(t))

  // in proper program these should be imported from prelude...
  val initialContext: Context = {
    Map(Syntax.ConVar("True") -> bool,
      Syntax.ConVar("False") -> bool,
      Syntax.ConVar("Nil") -> TypeScheme(List(α), list(α)),
      Syntax.ConVar("Cons") -> TypeScheme(List(α), α --> (list(α) --> list(α))),
      Syntax.ConVar("Pair") -> TypeScheme(List(α, β), α --> (β --> TypeConstructor(Syntax.TConVar("Pair"), List(α, β)))),
      Syntax.Var("+") -> (int --> (int --> int)),
      Syntax.Var("-") -> (int --> (int --> int)),
      Syntax.Var("==") -> (int --> (int --> bool)),
      Syntax.Var("ord") -> (char --> int),
      Syntax.Var("chr") -> (int --> char),
      Syntax.Var("yield") -> forall(α)(α --> dom(α)),
      Syntax.Var("&") -> forall(α, β)(dom(α) --> (dom(β) --> dom(β))))
  }
}