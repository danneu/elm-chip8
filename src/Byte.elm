module Byte exposing (..)

{-| This module wraps Int to represent unsigned bytes and bitwise arithmetic.

Byte math functions automatically wrap.

Justification:

  - `Int` is an error-prone way to represent bytes because you have to be vigilant about
    going out of bounds.
  - `Byte` is an infinitely more meaningful than `Int` when you indeed have bytes.

-}

import Bitwise


type Byte
    = Byte Int


fromBool : Bool -> Byte
fromBool bool =
    if bool then
        fromInt 1

    else
        fromInt 0


fromInt : Int -> Byte
fromInt n =
    Byte (Bitwise.and 0xFF n)


toInt : Byte -> Int
toInt (Byte n) =
    n


toBool : Byte -> Bool
toBool (Byte n) =
    n /= 0


isZero : Byte -> Bool
isZero (Byte x) =
    x == 0


and : Byte -> Byte -> Byte
and (Byte a) (Byte b) =
    Byte <| Bitwise.and a b


or : Byte -> Byte -> Byte
or (Byte a) (Byte b) =
    Byte <| Bitwise.or a b


xor : Byte -> Byte -> Byte
xor (Byte a) (Byte b) =
    Byte <| Bitwise.xor a b


{-| Bitwise complement (flips each bit).
-}
complement : Byte -> Byte
complement (Byte b) =
    fromInt (Bitwise.complement b)


add : Byte -> Byte -> Byte
add (Byte x) (Byte y) =
    fromInt (x + y)


addCarry : Byte -> Byte -> ( Byte, Bool )
addCarry (Byte x) (Byte y) =
    let
        sum =
            x + y
    in
    ( fromInt sum
    , sum > 255
    )


{-| Subtracts the second byte from the first.
-}
sub : Byte -> Byte -> Byte
sub (Byte x) (Byte y) =
    fromInt (x - y)


subCarry : Byte -> Byte -> ( Byte, Bool )
subCarry (Byte x) (Byte y) =
    ( fromInt (x - y)
    , x < y
    )


dec : Byte -> Byte
dec byte =
    sub byte (fromInt 1)



-- BITS


{-| Returns true if most significant bit is set.
-}
msb : Byte -> Bool
msb =
    getBit 7


{-| Returns true if least significant bit is set.
-}
lsb : Byte -> Bool
lsb =
    getBit 0


{-| Returns a `Bool` indicating whether or not the bit is set.
-}
getBit : Int -> Byte -> Bool
getBit n (Byte b) =
    (Bitwise.and 1 <| Bitwise.shiftRightBy n b) == 1


{-| Sets the nth bit of the `Byte`.
-}
set : Int -> Byte -> Byte
set n (Byte b) =
    Bitwise.shiftLeftBy n 1
        |> mask
        |> Bitwise.or b
        |> Byte


{-| Sets or resets the nth bit of the `Byte`.
-}
setIf : Int -> Bool -> Byte -> Byte
setIf n shouldSet byte =
    if shouldSet then
        set n byte

    else
        reset n byte


{-| Resets the nth bit of the `Byte`.
-}
reset : Int -> Byte -> Byte
reset n (Byte b) =
    Bitwise.shiftLeftBy n 1
        |> Bitwise.complement
        |> Bitwise.and b
        |> fromInt


shiftLeft : Byte -> Byte
shiftLeft ((Byte b) as byte) =
    fromInt <| Bitwise.shiftLeftBy 1 b


shiftLeftCarry : Byte -> ( Byte, Bool )
shiftLeftCarry byte =
    ( shiftLeft byte
    , msb byte
    )


shiftRight : Byte -> Byte
shiftRight ((Byte b) as byte) =
    b
        |> Bitwise.shiftRightBy 1
        |> Byte


shiftRightCarry : Byte -> ( Byte, Bool )
shiftRightCarry byte =
    ( shiftRight byte
    , lsb byte
    )



-- MASK


mask : Int -> Int
mask =
    Bitwise.and 0xFF


maskHigher : Int -> Int
maskHigher =
    Bitwise.and 0xF0


maskLower : Int -> Int
maskLower =
    Bitwise.and 0x0F
