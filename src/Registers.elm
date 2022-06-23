module Registers exposing (Registers, get, init, set)

{-| This module lets us set/get registers in a way that guarantees the
callsite that setting/getting will succeed. It's a big improvement over using Array[16] directly.
-}

import Array exposing (Array)
import Byte exposing (Byte)
import Register exposing (Register)


type Registers
    = Registers (Array Byte)


init : Registers
init =
    Array.initialize 16 (\_ -> Byte.fromInt 0)
        |> Registers


get : Register -> Registers -> Byte
get r (Registers array) =
    Array.get (Register.toInt r) array
        |> Maybe.withDefault (Byte.fromInt 0)


set : Register -> Byte -> Registers -> Registers
set r byte (Registers array) =
    Array.set (Register.toInt r) byte array
        |> Registers
