-- Copyright (c) 2010, Andreas Buechele, Florian Lorenzen, Judith Rohloff
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

-- This is a stub for the predefined types and methods of SL, so that we
-- can create the prelude by a combination of compilation and hand coding.

IMPORT EXTERN "std/_prelude" 

-------------------------------------
-- Booleans
-- (without Bool, there is no if-then-else)
PUBLIC DATA Bool = True | False

PUBLIC FUN not : Bool -> Bool
DEF not True = False
DEF not False = True

-------------------------------------
-- Function composition
PUBLIC FUN # : (b -> c) -> (a -> b) -> (a -> c)
DEF f # g = \ x . f (g x)

PUBLIC FUN id : a -> a
DEF id a = a

-- The representation of the undefined.
PUBLIC FUN error : String -> a
DEF EXTERN error = {| function(msg){throw msg} |} 

-------------------------------------
-- Base Types
-- (i.e. types that must exist because literals are mapped to them.)

DATA EXTERN Int
DATA EXTERN Real
DATA EXTERN Char
DATA EXTERN String

PUBLIC DATA Void = Void
DATA EXTERN DOM a

-------------------------------------
-- Monad functions

-- return / yield function 
PUBLIC FUN yield : a -> (DOM a)
DEF EXTERN yield =  {| _yield |}

-- shorthand for no operation. (equivalent to {| |} )
PUBLIC FUN noop : DOM Void
DEF noop = yield Void

-- bind function
PUBLIC FUN &= : DOM a -> (a -> DOM b) -> DOM b
DEF EXTERN &= = {| _bind |}

-- shorthand for bind with no argument
PUBLIC FUN & : DOM a -> DOM b -> DOM b
DEF EXTERN & = {| _bindnr |}

-------------------------------------
-- Arithmetics on Integers

PUBLIC FUN + : Int -> Int -> Int
DEF EXTERN + = {| _add |}

PUBLIC FUN - : Int -> Int -> Int
DEF EXTERN - = {| _sub |}

PUBLIC FUN * : Int -> Int -> Int
DEF EXTERN * = {| _mul |}

PUBLIC FUN / : Int -> Int -> Int
DEF EXTERN / = {| _div |}

PUBLIC FUN % : Int -> Int -> Int
DEF EXTERN % = {| function(a){return function(b){ return a%b; }} |}

PUBLIC FUN < : Int -> Int -> Bool
DEF EXTERN < = {| _lesser |}

PUBLIC FUN <= : Int -> Int -> Bool
DEF EXTERN <= = {| _leq |}

PUBLIC FUN == : Int -> Int -> Bool
DEF EXTERN == = {| _eq |}

PUBLIC FUN /= : Int -> Int -> Bool
DEF x /= y = not (x == y)

PUBLIC FUN >= : Int -> Int -> Bool
DEF EXTERN >= = {| _geq |}

PUBLIC FUN > : Int -> Int -> Bool
DEF EXTERN > = {| _greater |}

PUBLIC FUN neg : Int -> Int
DEF neg i = 0 - i

PUBLIC FUN iNaN : Int
DEF EXTERN iNaN = {| NaN |}
-- iNaN /= iNaN !!

PUBLIC FUN isNaN : Int -> Bool
DEF EXTERN isNaN = {| isNaN |}

-- String functions
PUBLIC FUN ++ : (String -> String -> String)
DEF EXTERN ++ = {| _adds |}

PUBLIC FUN strEq : String -> String -> Bool
DEF EXTERN strEq = {| _eq |}

-------------------------------------
-- Basic Conversions

PUBLIC FUN intToString : Int -> String
DEF EXTERN intToString = {| function(i){return i.toString();} |}
PUBLIC FUN intToChar : Int -> Char
DEF EXTERN intToChar = {| String.fromCharCode |}

PUBLIC FUN charToInt : Char -> Int
DEF EXTERN charToInt = {| function(c){return c.charCodeAt(0);} |}
PUBLIC FUN charToString : Char -> String
DEF EXTERN charToString = {| function(c){return c;} |}

PUBLIC FUN stringToInt : String -> Int
DEF EXTERN stringToInt = {| parseInt |}
PUBLIC FUN stringGetChar : String -> Int -> Char
DEF EXTERN stringGetChar = {| function(s){return function(i){
	if (s.length < i) {
		throw "stringGetChar failed: Char index out of bounds"
	} else {
		return s.charAt(i);
	}
}} |}

PUBLIC FUN boolToString : Bool -> String
DEF boolToString True = "True"
DEF boolToString False = "False"

PUBLIC DATA LAZY a = SUSPEND (Void -> a)
PUBLIC FUN force : LAZY a -> a
DEF force (SUSPEND f) = f Void 
