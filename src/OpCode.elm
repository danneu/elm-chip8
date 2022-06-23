module OpCode exposing (OpCode(..), fromInt, toString)

import Bitwise
import Byte exposing (Byte)
import Hex
import Register exposing (Register)


type
    OpCode
    -- 00E0 - CLS
    = Cls
      -- 00EE - RET
    | Ret
      -- 0nnn - SYS addr
    | Sys Int
      -- 1nnn - JP addr
    | Jp Int
      -- 2nnn: CALL addr
    | Call Int
      -- 3xkk - SE Vx, byte
    | Se Register Byte
      -- 4xkk - SNE Vx, byte
    | Sne Register Byte
      -- 5xy0 - SE Vx, Vy
    | Sev Register Register
      -- 6xkk - LD Vx, byte
    | Ldv Register Byte
      -- 7xkk - ADD Vx, byte
    | Add Register Byte
      -- 8xy0 - LD Vx, Vy
    | Ldvv Register Register
      -- 8xy1 - OR Vx, Vy
    | Or Register Register
      -- 8xy2 - AND Vx, Vy
    | And Register Register
      -- 8xy3 - XOR Vx, Vy
    | Xor Register Register
      -- 8xy4 - ADD Vx, Vy
    | Addvv Register Register
      -- 8xy5 - SUB Vx, Vy
    | Sub Register Register
      -- 8xy6 - SHR Vx {, Vy}
    | Shr Register Register
      -- 8xy7 - SUBN Vx, Vy
    | Subn Register Register
      -- 8xyE - SHL Vx {, Vy}
    | Shl Register Register
      -- 9xy0 - SNE Vx, Vy
    | Snev Register Register
      -- Annn - LD I, addr
    | Ldi Int
      -- Bnnn - JP V0, addr
    | Jpv Int
      -- Cxkk - RND Vx, byte
    | Rnd Register Byte
      -- Dxyn - DRW Vx, Vy, nibble
    | Drw Register Register Int
      -- Ex9E - SKP Vx
    | Skp Register
      -- ExA1 - SKNP Vx
    | Sknp Register
      -- Fx07 - LD Vx, DT
    | LoadDelayTimer Register
      -- Fx0A - LD Vx, K
    | Ldvk Register
      -- Fx15 - LD DT, Vx
    | SetDelayTimer Register
      -- Fx18 - LD ST, Vx
    | SetSoundTimer Register
      -- Fx1E - ADD I, Vx
    | Addiv Register
      -- Fx29 - LD F, Vx
    | Ldfv Register
      -- Fx33 - LD B, Vx
    | Ldbv Register
      -- Fx55 - LD [I], Vx
    | Ldiv Register
      -- Fx65 - LD Vx, [I]
    | Ldvi Register


toString : OpCode -> String
toString op =
    let
        toHex n_ =
            "0x" ++ (String.toUpper <| String.padLeft 2 '0' (Hex.toString n_))

        byteToHex b =
            toHex (Byte.toInt b)
    in
    case op of
        -- 00E0 - CLS
        Cls ->
            "CLS  "

        -- 00EE - RET
        Ret ->
            "RET  "

        -- 0nnn - SYS addr
        Sys addr ->
            "SYS  " ++ toHex addr

        -- 1nnn - JP addr
        Jp addr ->
            "JP   " ++ toHex addr

        -- 2nnn: CALL addr
        Call addr ->
            "CALL " ++ toHex addr

        -- 3xkk - SE Vx, byte
        Se x kk ->
            "SE   V" ++ Hex.toString (Register.toInt x) ++ ", " ++ byteToHex kk

        -- 4xkk - SNE Vx, byte
        Sne x kk ->
            "SNE  V" ++ Hex.toString (Register.toInt x) ++ ", " ++ byteToHex kk

        -- 5xy0 - SE Vx, Vy
        Sev x y ->
            "SE   V" ++ Hex.toString (Register.toInt x) ++ ", V" ++ Hex.toString (Register.toInt y)

        -- 6xkk - LD Vx, byte
        Ldv x kk ->
            "LD   V" ++ Hex.toString (Register.toInt x) ++ ", " ++ byteToHex kk

        -- 7xkk - ADD Vx, byte
        Add x kk ->
            "ADD  V" ++ Hex.toString (Register.toInt x) ++ ", " ++ byteToHex kk

        -- 8xy0 - LD Vx, Vy
        Ldvv x y ->
            "LD   V" ++ Hex.toString (Register.toInt x) ++ ", V" ++ Hex.toString (Register.toInt y)

        -- 8xy1 - OR Vx, Vy
        Or x y ->
            "OR   V" ++ Hex.toString (Register.toInt x) ++ ", V" ++ Hex.toString (Register.toInt y)

        -- 8xy2 - AND Vx, Vy
        And x y ->
            "AND  V" ++ Hex.toString (Register.toInt x) ++ ", V" ++ Hex.toString (Register.toInt y)

        -- 8xy3 - XOR Vx, Vy
        Xor x y ->
            "XOR  V" ++ Hex.toString (Register.toInt x) ++ ", V" ++ Hex.toString (Register.toInt y)

        -- 8xy4 - ADD Vx, Vy
        Addvv x y ->
            "ADD  V" ++ Hex.toString (Register.toInt x) ++ ", V" ++ Hex.toString (Register.toInt y)

        -- 8xy5 - SUB Vx, Vy
        Sub x y ->
            "SUB  V" ++ Hex.toString (Register.toInt x) ++ ", V" ++ Hex.toString (Register.toInt y)

        -- 8xy6 - SHR Vx {, Vy}
        Shr x y ->
            "SHR  V" ++ Hex.toString (Register.toInt x) ++ " {, V" ++ Hex.toString (Register.toInt y) ++ "}"

        -- 8xy7 - SUBN Vx, Vy
        Subn x y ->
            "SUBN V" ++ Hex.toString (Register.toInt x) ++ ", V" ++ Hex.toString (Register.toInt y)

        -- 8xyE - SHL Vx {, Vy}
        Shl x y ->
            "SHL  V" ++ Hex.toString (Register.toInt x) ++ " {, V" ++ Hex.toString (Register.toInt y) ++ "}"

        -- 9xy0 - SNE Vx, Vy
        Snev a b ->
            "SNE  V" ++ Hex.toString (Register.toInt a) ++ ", V" ++ Hex.toString (Register.toInt b)

        -- Annn - LD I, addr
        Ldi addr ->
            "LD   I, " ++ toHex addr

        -- Bnnn - JP V0, addr
        Jpv addr ->
            "JP   V0, " ++ toHex addr

        -- Cxkk - RND Vx, byte
        Rnd x kk ->
            "RND  V" ++ Hex.toString (Register.toInt x) ++ ", " ++ byteToHex kk

        -- Dxyn - DRW Vx, Vy, nibble
        Drw x y nibble ->
            "DRW  V" ++ Hex.toString (Register.toInt x) ++ ", V" ++ Hex.toString (Register.toInt y) ++ ", " ++ toHex nibble

        -- Ex9E - SKP Vx
        Skp x ->
            "SKP  V" ++ Hex.toString (Register.toInt x)

        -- ExA1 - SKNP Vx
        Sknp x ->
            "SKNP V" ++ Hex.toString (Register.toInt x)

        -- LD Vx, DT
        LoadDelayTimer x ->
            "LD   V" ++ Hex.toString (Register.toInt x) ++ ", DT"

        -- Fx0A - LD Vx, K
        Ldvk x ->
            "LD   V" ++ Hex.toString (Register.toInt x) ++ ", K"

        -- Fx15 - LD DT, Vx
        SetDelayTimer x ->
            "LD   DT, V" ++ Hex.toString (Register.toInt x)

        -- Fx18 - LD ST, Vx
        SetSoundTimer x ->
            "LD   ST, V" ++ Hex.toString (Register.toInt x)

        -- Fx1E - ADD I, Vx
        Addiv x ->
            "ADD  I, V" ++ Hex.toString (Register.toInt x)

        -- Fx29 - LD F, Vx
        Ldfv x ->
            "LD   F, V" ++ Hex.toString (Register.toInt x)

        -- Fx33 - LD B, Vx
        Ldbv x ->
            "LD   B, V" ++ Hex.toString (Register.toInt x)

        -- Fx55 - LD [I], Vx
        Ldiv x ->
            "LD   [I], V" ++ Hex.toString (Register.toInt x)

        -- Fx65 - LD Vx, [I]
        Ldvi x ->
            "LD   V" ++ Hex.toString (Register.toInt x) ++ ", [I]"


fromInt : Int -> Maybe OpCode
fromInt n =
    if n == 0xE0 then
        Just Cls

    else if n == 0xEE then
        Just Ret

    else if Bitwise.and 0xF000 n == 0x00 then
        Just (Sys (Bitwise.and n 0x0FFF))

    else if Bitwise.and 0xF000 n == 0x1000 then
        Just (Jp (Bitwise.and n 0x0FFF))

    else if Bitwise.and 0xF000 n == 0x2000 then
        Just (Call (Bitwise.and n 0x0FFF))

    else if Bitwise.and 0xF000 n == 0x3000 then
        -- 3xkk - SE Vx, byte
        Maybe.map2 Se
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            -- (Just (Bitwise.and 0xFF n))
            (Just (Byte.fromInt n))

    else if Bitwise.and 0xF000 n == 0x4000 then
        -- 4xkk - SNE Vx, byte
        Maybe.map2 Sne
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            -- (Just (Bitwise.and 0xFF n))
            (Just (Byte.fromInt n))

    else if Bitwise.and 0xF00F n == 0x5000 then
        -- 5xy0 - SE Vx, Vy
        -- TODO: instead of // 0x0100, shift right
        Maybe.map2 Sev
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))

    else if Bitwise.and 0xF000 n == 0x6000 then
        -- 6xkk - LD Vx, byte
        Maybe.map2 Ldv
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            -- (Just (Bitwise.and 0xFF n))
            (Just (Byte.fromInt n))

    else if Bitwise.and 0xF000 n == 0x7000 then
        -- 7xkk - ADD Vx, byte
        Maybe.map2 Add
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            -- (Just (Bitwise.and 0xFF n))
            (Just (Byte.fromInt n))

    else if Bitwise.and 0xF00F n == 0x8000 then
        -- 8xy0 - LD Vx, Vy
        Maybe.map2 Ldvv
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))

    else if Bitwise.and 0xF00F n == 0x8001 then
        -- 8xy1 - OR Vx, Vy
        Maybe.map2 Or
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))

    else if Bitwise.and 0xF00F n == 0x8002 then
        -- 8xy2 - AND Vx, Vy
        Maybe.map2 And
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))

    else if Bitwise.and 0xF00F n == 0x8003 then
        -- 8xy3 - XOR Vx, Vy
        Maybe.map2 Xor
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))

    else if Bitwise.and 0xF00F n == 0x8004 then
        -- 8xy4 - ADD Vx, Vy
        Maybe.map2 Addvv
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))

    else if Bitwise.and 0xF00F n == 0x8005 then
        -- 8xy5 - SUB Vx, Vy
        Maybe.map2 Sub
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))

    else if Bitwise.and 0xF00F n == 0x8006 then
        -- 8xy6 - SHR Vx {, Vy}
        Maybe.map2 Shr
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))

    else if Bitwise.and 0xF00F n == 0x8007 then
        -- 8xy7 - SUBN Vx, Vy
        Maybe.map2 Subn
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))

    else if Bitwise.and 0xF00F n == 0x800E then
        -- 8xyE - SHL Vx {, Vy}
        Maybe.map2 Shl
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))

    else if Bitwise.and 0xF000 n == 0x9000 then
        -- 9xy0 - SNE Vx, Vy
        Maybe.map2 Snev
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))

    else if Bitwise.and 0xF000 n == 0xA000 then
        -- Annn - LD I, addr
        Just (Ldi (Bitwise.and 0x0FFF n))

    else if Bitwise.and 0xF000 n == 0xB000 then
        -- Bnnn - JP V0, addr
        let
            addr =
                Bitwise.and 0x0FFF n
        in
        Just (Jpv addr)

    else if Bitwise.and 0xF000 n == 0xC000 then
        -- Cxkk - RND Vx, byte
        Maybe.map2 Rnd
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            -- (Just (Bitwise.and 0xFF n))
            (Just (Byte.fromInt n))

    else if Bitwise.and 0xF000 n == 0xD000 then
        -- Dxyn - DRW Vx, Vy, nibble
        Maybe.map3 Drw
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))
            (Register.fromInt (Bitwise.and 0xF0 n // 0x10))
            (Just (Bitwise.and 0x0F n))

    else if Bitwise.and 0xF0FF n == 0xE09E then
        -- Ex9E - SKP Vx
        Maybe.map Skp
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))

    else if Bitwise.and 0xF0FF n == 0xE0A1 then
        -- ExA1 - SKNP Vx
        Maybe.map Sknp
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))

    else if Bitwise.and 0xF0FF n == 0xF007 then
        -- Fx07 - LD Vx, DT
        Maybe.map LoadDelayTimer
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))

    else if Bitwise.and 0xF0FF n == 0xF00A then
        -- Fx0A - LD Vx, K
        Maybe.map Ldvk
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))

    else if Bitwise.and 0xF0FF n == 0xF015 then
        -- Fx15 - LD DT, Vx
        Maybe.map SetDelayTimer
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))

    else if Bitwise.and 0xF0FF n == 0xF018 then
        -- Fx18 - LD ST, Vx
        Maybe.map SetSoundTimer
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))

    else if Bitwise.and 0xF0FF n == 0xF01E then
        -- Fx1E - ADD I, Vx
        Maybe.map Addiv
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))

    else if Bitwise.and 0xF0FF n == 0xF029 then
        -- Fx29 - LD F, Vx
        Maybe.map Ldfv
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))

    else if Bitwise.and 0xF0FF n == 0xF033 then
        -- Fx33 - LD B, Vx
        Maybe.map Ldbv
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))

    else if Bitwise.and 0xF0FF n == 0xF055 then
        -- Fx55 - LD [I], Vx
        Maybe.map Ldiv
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))

    else if Bitwise.and 0xF0FF n == 0xF065 then
        -- Fx65 - LD Vx, [I]
        Maybe.map Ldvi
            (Register.fromInt (Bitwise.and 0x0F00 n // 0x0100))

    else
        -- Debug.todo ("[OpCode.fromInt] unimplemented n: 0x" ++ Hex.toString n)
        Nothing
