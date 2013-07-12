-- Copyright (c) 2012, Technische Universität Berlin
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:

--   * Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above
--     copyright notice, this list of conditions and the following
--     disclaimer in the documentation and/or other materials provided
--     with the distribution.
--   * Neither the name of the TU Berlin nor the names of its
--     contributors may be used to endorse or promote products derived
--     from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


------------------------------------------------------------------------
-- Tuples
------------------------------------------------------------------------

DATA T2 a b     = T2 a b
DATA T3 a b c   = T3 a b c
DATA T4 a b c d = T4 a b c d

DEF fst2 x = CASE x OF T2 a b THEN a
DEF snd2 x = CASE x OF T2 a b THEN b

DEF fst3 x = CASE x OF T3 a b c THEN a
DEF snd3 x = CASE x OF T3 a b c THEN b
DEF trd3 x = CASE x OF T3 a b c THEN c

DEF fst4 x = CASE x OF T4 a b c d THEN a
DEF snd4 x = CASE x OF T4 a b c d THEN b
DEF trd4 x = CASE x OF T4 a b c d THEN c
DEF fth4 x = CASE x OF T4 a b c d THEN d

DEF showT2 showA showB (T2 a b) = "(" +s (showA a) +s ", " +s (showB b) +s ")"

DEF showT3 showA showB showC (T3 a b c) = "(" +s (showA a) +s ", " +s (showB b) +s ", " +s (showC c) +s ")"

DEF showT4 showA showB showC showD (T4 a b c d) = "(" +s (showA a) +s ", " +s (showB b) +s ", " +s (showC c) +s ", " +s (showD d) +s ")"


------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------

DEF singleton x = Cons x Nil

DEF concat xs = foldl append Nil xs

DEF append Nil         ys = ys
DEF append (Cons x xs) ys = Cons x (append xs ys)

DEF map f Nil         = Nil
DEF map f (Cons x xs) = Cons (f x) (map f xs)

DEF foldl f z Nil         = z
DEF foldl f z (Cons x xs) = foldl f (f z x) xs

DEF foldr f z Nil         = z
DEF foldr f z (Cons x xs) = f x (foldr f z xs)

DEF filter p Nil = Nil
DEF filter p (Cons x xs) = IF p x THEN Cons x (filter p xs)
                                  ELSE filter p xs

DEF break p Nil = T2 Nil Nil
DEF break p (Cons x xs) = IF p x THEN T2 Nil xs
                                 ELSE CASE break p xs 
                                        OF T2 ys zs THEN T2 (Cons x ys) zs

DEF reverse xs = foldl (flip (\ x. \ y. Cons x y)) Nil xs

DEF elem eq x Nil         = False
DEF elem eq x (Cons y ys) = IF eq y x THEN True
                                      ELSE elem eq x ys

DEF showList show Nil = "[]"
DEF showList show (Cons x xs) = LET showList1 = \ (Cons x xs) . CASE xs
                                                                  OF Nil       THEN (show x) +s "]"
                                                                  OF Cons y ys THEN (show x) +s "," +s (showList1 xs)
                       IN "[" +s (showList1 xs)

DEF head (Cons x xs) = x

DEF tail (Cons x xs) = xs


------------------------------------------------------------------------
-- Boolean functions
------------------------------------------------------------------------

DEF and x y = CASE x
                OF False THEN False
                OF True  THEN CASE y
                                OF False THEN False
                                OF True  THEN True

DEF or x y = CASE x
               OF True  THEN True
               OF False THEN CASE y
                               OF False THEN False
                               OF True  THEN True

DEF not x = CASE x
              OF True  THEN False
              OF False THEN True

DEF xor x y = CASE x
                OF True  THEN (CASE y
                                 OF True  THEN False
                                 OF False THEN True)
                OF False THEN (CASE y
                                 OF True  THEN True
                                 OF False THEN False)

DEF eql x y = CASE x
                OF True  THEN (CASE y
                                 OF True  THEN True
                                 OF False THEN False)
                OF False THEN (CASE y
                                 OF True  THEN False
                                 OF False THEN True)

DEF showBool x = CASE x
                   OF True  THEN "True"
                   OF False THEN "False"


------------------------------------------------------------------------
-- Integer functions
------------------------------------------------------------------------

DEF neg n = -1 * n

DEF mod m n = IF m < n THEN m ELSE mod (m - n) n

DEF abs n = IF n < 0 THEN neg n ELSE n

DEF even x = IF x == 0 THEN True  ELSE odd(x - 1)

DEF odd x = IF x == 1 THEN True ELSE even(x - 1)

DEF showInt m = IF m < 0 THEN "-" +s (showInt1 (neg m))
                         ELSE showInt1 m

DEF showInt1 m = IF and (m >= 0) (m < 10)
                 THEN showDigit m
                 ELSE IF m >= 10
                      THEN LET d = mod m 10
                               r = m / 10
                           IN (showInt1 r) +s (showDigit d)
                      ELSE ""

DEF showDigit d = CASE d == 0
                    OF True  THEN "0"
                    OF False THEN
                  CASE d == 1
                    OF True  THEN "1"
                    OF False THEN
                  CASE d == 2
                    OF True  THEN "2"
                    OF False THEN
                  CASE d == 3
                    OF True  THEN "3"
                    OF False THEN
                  CASE d == 4
                    OF True  THEN "4"
                    OF False THEN
                  CASE d == 5
                    OF True  THEN "5"
                    OF False THEN
                  CASE d == 6
                    OF True  THEN "6"
                    OF False THEN
                  CASE d == 7
                    OF True  THEN "7"
                    OF False THEN
                  CASE d == 8
                    OF True  THEN "8"
                    OF False THEN
                  CASE d == 9
                    OF True  THEN "9"
                    OF False THEN "X"


------------------------------------------------------------------------
-- Character functions
------------------------------------------------------------------------

DEF showChar c = singleton c


------------------------------------------------------------------------
-- Various functions
------------------------------------------------------------------------

DEF id x = x

DEF const x dummy = x

DEF composition f g =  \ x . f (g x)

DEF flip f x y =  f y x
