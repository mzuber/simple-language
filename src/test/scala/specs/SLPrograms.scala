/*
 * Copyright (c) 2012, Technische Universität Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided
 *   with the distribution.
 * - Neither the name of the TU Berlin nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package de.tuberlin.uebb.sl2.tests.specs

/**
  * SL programs for testing purposes.
  */ 
trait SLPrograms {

  val constants = """
  |DEF l1 = LET c2 = 2 IN c1
  |DEF l2 = LET c2 = 2 IN c3
  |DEF c3 = c1 + c2
  |DEF c1 = c2
  |DEF c2 = 1
  |DEF l3 = LET c2 = 2 IN c2 + c1
  |DEF l4 = (LET c2 = 3 IN \y.c2+y)(LET c2 = 5 IN c2)
  """.stripMargin


  val multipleParams = "DEF add x y z = x + y + z"


  val concat = """
  |DEF xs        ++ Nil = xs
  |DEF Nil       ++ xs  = xs
  |DEF (Cons x xs) ++ ys  = Cons x (xs ++ ys)
    """.stripMargin


  val range = "DEF range n = IF n <= 0 THEN Cons 0 Nil ELSE Cons n (range (n - 1))"

  
  val filter = """
  |DEF filter (Nil)       p = Nil
  |DEF filter (Cons x xs) p = IF p x THEN Cons x (filter xs p) ELSE filter xs p
  |
  |DEF filterNot xs p = filter xs (\ x . not (p x))
  """.stripMargin


  val reverse = """
  |DEF reverse Nil = Nil
  |DEF reverse xs  = reverseHelper Nil xs
  |
  |DEF reverseHelper r Nil       = r
  |DEF reverseHelper r (Cons x xs) = reverseHelper (Cons x r) xs
  """.stripMargin


  val sort = range + concat + filter + reverse + """
  |DEF quicksort Nil        = Nil
  |DEF quicksort (Cons x Nil) = Cons x Nil
  |DEF quicksort (Cons x xs) = LET lessThanX = \ y . y < x
  |                          IN (filter xs lessThanX) ++ (Cons x (filterNot xs lessThanX))
  """.stripMargin


  val caseWithBuiltIn = """
  | DEF f x = CASE x
  |	        OF True  THEN 0
  |	        OF False THEN 2
  """.stripMargin


  val caseWithCustomType = """
  |DATA Color = Red | Green | Blue | Custom Int
  |
  |DEF choose True c = Custom c
  |DEF choose x c = IF c == 0 THEN Red
  |	            ELSE IF c == 1 THEN Green
  |		         ELSE Blue
  |
  |DEF f b c = CASE choose b c
  |	         OF Red      THEN -1
  |	         OF Green    THEN -2
  |              OF Blue     THEN -3
  |	         OF Custom i THEN i
  """.stripMargin

  val nestedConditional = "DEF f a b c = IF (IF a > 2 THEN b ELSE c) > 3 THEN 0 ELSE 1"


  val lambdaPatterns = """
  | DEF f = (\ (Cons a b) (Cons x (Cons Nil Nil)) y True . a / y)
  |         (LET first = 1593
  |  	         rest  = Cons 1 Nil
  |          IN Cons first rest)
  |         (Cons Nil (Cons Nil Nil))
  |         (LET a = -9 IN a / 3)
  |         True
  """.stripMargin


  val shadowedLocalDef = """
  | DEF f = LET s = 0
  |         IN (\ x . s * x) (LET s = 3 IN s)
  """.stripMargin


  val nestedLet = """
  | DEF g a b c d e = a + b + c + d + e
  |
  | DEF result = LET s  = 5
  |                  f1 = g s
  |              IN LET s  = 40
  |                     f2 = f1 s
  |                 IN LET s  = 300
  |                        f3 = f2 s
  |                    IN LET s  = 2000
  |                           f4 = f3 s
  |                       IN LET s  = 10000
  |                              f5 = f4 s
  |                          IN f5
  """.stripMargin


  val shadowedPatternVar = """
  | DEF f (Cons b a) v = LET g = \ a . a + v
  |                      IN g b
  """.stripMargin


  val shadowedTopLevelNames = """
  | DEF h = 10
  |
  | DEF g = 20
  |
  | DEF f g h = \ x . g + h
  """.stripMargin


  val ulam = """
  | DEF ulam a = LET f = \ x . g (3 * x + 1)
  |                  g = \ x . IF x == 1 THEN 1 ELSE IF x / 2 * 2 == x THEN g (x / 2) ELSE f x
  |              IN g a
  """.stripMargin


  val partialApplication = """
  | DEF f a b = a + b
  |
  | DEF g a = Cons (\x . f x a) Nil
  |
  | DEF first (Cons a b) = a
  | DEF rest (Cons a b) = b
  |
  | DEF call a c = (first c) a
  """.stripMargin


  val lateMatch = """
  | DEF f (Cons a b) x (Cons c d) = 1
  | DEF f z          x (Cons f g) = 2
  | DEF f y          x Nil        = 3
  """.stripMargin

  val mixedPatterns = """
  | DATA Color = Red | Blue | Green
  |
  | DEF f Red  b     = 0
  | DEF f Blue b     = 1
  | DEF f c    True  = 2
  | DEF f c    False = 3
  """.stripMargin


  var overlappingPatterns = """
  | DEF f a b c = 1 
  | DEF f a b c = 2
  """.stripMargin


  var shadowedVars = """
  | DEF f = LET v = 10
  |             x = (LET v = 20 IN \ x y . v * x) (LET v = -30 IN v) v
  |         IN v + x
  """.stripMargin
}
