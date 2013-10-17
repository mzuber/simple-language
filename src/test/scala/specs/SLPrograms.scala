/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
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

  val concat = """
  |DEF xs            +++ L.Nil = xs
  |DEF L.Nil         +++ xs  = xs
  |DEF (L.Cons x xs) +++ ys  = L.Cons x (xs +++ ys)
    """.stripMargin


  val range = """
  |IMPORT "std/list" AS L
  |DEF range n = IF n <= 0 THEN L.Cons 0 L.Nil ELSE L.Cons n (range (n - 1))"""

  
  val filter = """
  |DEF filter (L.Nil)       p = L.Nil
  |DEF filter (L.Cons x xs) p = IF p x THEN L.Cons x (filter xs p) ELSE filter xs p
  |
  |DEF filterNot xs p = filter xs (\ x . not (p x))
  """.stripMargin


  val reverse = """
  |DEF reverse L.Nil = L.Nil
  |DEF reverse xs  = reverseHelper L.Nil xs
  |
  |DEF reverseHelper r L.Nil         = r
  |DEF reverseHelper r (L.Cons x xs) = reverseHelper (L.Cons x r) xs
  """.stripMargin


  val sort = range + concat + filter + reverse + """
  |DEF quicksort L.Nil            = L.Nil
  |DEF quicksort (L.Cons x L.Nil) = L.Cons x L.Nil
  |DEF quicksort (L.Cons x xs)    = LET lessThanX = \ y . y < x
  |                                 IN (filter xs lessThanX) +++ (L.Cons x (filterNot xs lessThanX))
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
  | IMPORT "std/list" AS L
  | DEF f = (\ (L.Cons a b) (L.Cons x (L.Cons L.Nil L.Nil)) y True . a / y)
  |         (LET first = 1593
  |  	         rest  = L.Cons 1 L.Nil
  |          IN L.Cons first rest)
  |         (L.Cons L.Nil (L.Cons L.Nil L.Nil))
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
  | IMPORT "std/list" AS L
  | DEF f (L.Cons b a) v = LET g = \ a . a + v
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
  | IMPORT "std/list" AS L
  | DEF f a b = a + b
  |
  | DEF g a = L.Cons (\x . f x a) L.Nil
  |
  | DEF first (L.Cons a b) = a
  | DEF rest (L.Cons a b) = b
  |
  | DEF call a c = (first c) a
  """.stripMargin


  val lateMatch = """
  | IMPORT "std/list" AS L
  | DEF f (L.Cons a b) x (L.Cons c d) = 1
  | DEF f z            x (L.Cons f g) = 2
  | DEF f y            x L.Nil        = 3
  """.stripMargin

  val mixedPatterns = """
  | DATA Color = Red | Blue | Green
  |
  | DEF f Red  b     = 0
  | DEF f Blue b     = 1
  | DEF f c    True  = 2
  | DEF f c    False = 3
  """.stripMargin


  val overlappingPatterns = """
  | DEF f a b c = 1 
  | DEF f a b c = 2
  """.stripMargin


  val shadowedVars = """
  | DEF f = LET v = 10
  |             x = (LET v = 20 IN \ x y . v * x) (LET v = neg 30 IN v) v
  |         IN v + x
  """.stripMargin
}
