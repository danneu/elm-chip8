module Cpu exposing (Cpu, Error, clearKeys, fromProgram, keyDown, keyUp, step, tick, withSpeed)

import Array exposing (Array)
import Bitwise
import Byte exposing (Byte)
import FrameBuffer exposing (FrameBuffer)
import Key
import OpCode exposing (OpCode)
import Register exposing (Register)
import Registers exposing (Registers)
import Set exposing (Set)


type Error
    = -- Fetch
      UnknownOpCode Int
    | ProgramCounterOutOfRange Int
      -- Execution
    | StackUnderflow
    | StackOverflow


type alias Cpu =
    { mem : Array Byte -- 4KB (4096 bytes)
    , v : Registers
    , i : Int
    , stack : List Int -- Max size: 16
    , pc : Int
    , soundTimer : Byte
    , delayTimer : Byte
    , frameBuf : FrameBuffer
    , seed : Int
    , awaitingKeyPress : Maybe Register
    , keysDown : Set Int
    , steps : Int
    , opsPerSecond : Float
    }


new : Int -> Cpu
new seed =
    { mem =
        Array.append
            reservedMem
            (Array.initialize 4069 (\_ -> Byte.fromInt 0))
            |> arrayTake 4096
    , v = Registers.init
    , soundTimer = Byte.fromInt 0
    , delayTimer = Byte.fromInt 0
    , pc = 0x0200
    , i = 0
    , stack = []
    , frameBuf = FrameBuffer.empty
    , seed = seed
    , awaitingKeyPress = Nothing
    , keysDown = Set.empty
    , steps = 0
    , opsPerSecond = 500
    }


{-| Change cpu speed.
-}
withSpeed : Float -> Cpu -> Cpu
withSpeed speed cpu =
    { cpu | opsPerSecond = speed }


{-| Tell the cpu about user input.

If the cpu is waiting for user interaction, this function will also
advance the cpu.

-}
keyDown : Key.Key -> Cpu -> Cpu
keyDown key cpu =
    let
        keyValue =
            Key.toInt key

        cpu1 =
            case cpu.awaitingKeyPress of
                Nothing ->
                    cpu

                Just reg ->
                    { cpu
                        | v = Registers.set reg (Byte.fromInt keyValue) cpu.v
                        , awaitingKeyPress = Nothing
                        , pc = cpu.pc + 2
                    }
    in
    { cpu1 | keysDown = Set.insert keyValue cpu.keysDown }


{-| Tell the cpu when user stops pressing a button.
-}
keyUp : Key.Key -> Cpu -> Cpu
keyUp key cpu =
    { cpu | keysDown = Set.remove (Key.toInt key) cpu.keysDown }


{-| Clear all user input.
-}
clearKeys : Cpu -> Cpu
clearKeys cpu =
    { cpu | keysDown = Set.empty }


{-| Chip-8's built-in fonts to be stored starting at addr 0x00.
-}
fontSprites : List Byte
fontSprites =
    List.concat
        [ [ 0xF0, 0x90, 0x90, 0x90, 0xF0 ] -- 0
        , [ 0x20, 0x60, 0x20, 0x20, 0x70 ] -- 1
        , [ 0xF0, 0x10, 0xF0, 0x80, 0xF0 ] -- 2
        , [ 0xF0, 0x10, 0xF0, 0x10, 0xF0 ] -- 3
        , [ 0x90, 0x90, 0xF0, 0x10, 0x10 ] -- 4
        , [ 0xF0, 0x80, 0xF0, 0x10, 0xF0 ] -- 5
        , [ 0xF0, 0x80, 0xF0, 0x90, 0xF0 ] -- 6
        , [ 0xF0, 0x10, 0x20, 0x40, 0x40 ] -- 7
        , [ 0xF0, 0x90, 0xF0, 0x90, 0xF0 ] -- 8
        , [ 0xF0, 0x90, 0xF0, 0x10, 0xF0 ] -- 9
        , [ 0xF0, 0x90, 0xF0, 0x90, 0x90 ] -- A
        , [ 0xE0, 0x90, 0xE0, 0x90, 0xE0 ] -- B
        , [ 0xF0, 0x80, 0x80, 0x80, 0xF0 ] -- C
        , [ 0xE0, 0x90, 0x90, 0x90, 0xE0 ] -- D
        , [ 0xF0, 0x80, 0xF0, 0x80, 0xF0 ] -- E
        , [ 0xF0, 0x80, 0xF0, 0x80, 0x80 ] -- F
        ]
        |> List.map Byte.fromInt


{-| 512 bytes of default memory at addr 0x00 to 0x1FF.

Contains bit font.

-}
reservedMem : Array Byte
reservedMem =
    Array.append
        (Array.fromList fontSprites)
        (Array.initialize 512 (\_ -> Byte.fromInt 0))
        |> arrayTake 512


fromProgram : Int -> List Int -> Cpu
fromProgram seed program =
    load program (new seed)


load : List Int -> Cpu -> Cpu
load bytes cpu =
    let
        mem =
            Array.append
                (Array.append reservedMem (Array.fromList (List.map Byte.fromInt bytes)))
                (Array.initialize 4096 (\_ -> Byte.fromInt 0))
                |> arrayTake 4096
    in
    { cpu | mem = mem }


{-| Uses the program counter to get the next 16-bit opcode that will be executed.
-}
fetchOp : Cpu -> Maybe Int
fetchOp cpu =
    Maybe.map2
        (\hi lo ->
            Bitwise.or (Bitwise.shiftLeftBy 8 (Byte.toInt hi)) (Byte.toInt lo)
        )
        (Array.get cpu.pc cpu.mem)
        (Array.get (cpu.pc + 1) cpu.mem)



-- Array.get cpu.pc cpu.mem
--     |> Maybe.andThen
--         (\hi ->
--             Array.get (cpu.pc + 1) cpu.mem
--                 |> Maybe.map
--                     (\lo ->
--                     )
--         )


stepManyHelp : Int -> List OpCode -> Cpu -> Result Error ( List OpCode, Cpu )
stepManyHelp n ops cpu =
    if n <= 0 then
        Ok ( ops, cpu )

    else
        case step1 cpu of
            Err e ->
                Err e

            Ok ( op, newCpu ) ->
                stepManyHelp (n - 1) (op :: ops) newCpu


{-| Step through 1+ steps of program execution.

Short-circuits on error.

-}
step : Int -> Cpu -> Result Error ( List OpCode, Cpu )
step n cpu =
    stepManyHelp n [] cpu


{-| Step through a single fetch/decode execution.
-}
step1 : Cpu -> Result Error ( OpCode, Cpu )
step1 cpu =
    case fetchOp cpu of
        Nothing ->
            Err (ProgramCounterOutOfRange cpu.pc)

        Just code ->
            case OpCode.fromInt code of
                Nothing ->
                    Err (UnknownOpCode code)

                Just op ->
                    case execute op cpu of
                        Err e ->
                            Err e

                        Ok c ->
                            let
                                steps =
                                    cpu.steps + 1

                                -- steps per 1/60 second
                                stepsPerFrame =
                                    -- FIXME: Too naive.
                                    (c.opsPerSecond / 60)
                                        |> ceiling
                                        |> Basics.max 1
                            in
                            -- Ew
                            c
                                |> (\x -> { x | steps = steps })
                                |> (if remainderBy stepsPerFrame steps == 0 then
                                        decrementTimers

                                    else
                                        identity
                                   )
                                |> Tuple.pair op
                                |> Ok


decrementTimers : Cpu -> Cpu
decrementTimers cpu =
    { cpu
        | delayTimer =
            if Byte.isZero cpu.delayTimer then
                cpu.delayTimer

            else
                Byte.dec cpu.delayTimer
        , soundTimer =
            if Byte.isZero cpu.soundTimer then
                cpu.soundTimer

            else
                Byte.dec cpu.soundTimer
    }


{-| Calls `step` repeatedly according to deltaTime.
-}
tick : Float -> Cpu -> Result Error ( List OpCode, Cpu )
tick deltaTime cpu =
    let
        instrCount =
            Basics.max 1 (floor ((deltaTime / 1000) * cpu.opsPerSecond))
    in
    -- step instrCount cpu
    step instrCount cpu


{-| Generate a pseudorandom number from a seed.

The return value can be used for the next seed.

Impl: <http://cs.uccs.edu/~cs591/bufferOverflow/glibc-2.2.4/stdlib/random_r.c>

-}
genRandNumber : Int -> Int
genRandNumber seed =
    let
        m : Int
        m =
            2 ^ 31 - 1

        a : Int
        a =
            1103515245

        c : Int
        c =
            12345
    in
    modBy m (a * seed + c)


execute : OpCode -> Cpu -> Result Error Cpu
execute op cpu =
    case op of
        -- 00E0 - CLS
        OpCode.Cls ->
            Ok
                { cpu
                    | frameBuf = FrameBuffer.empty
                    , pc = cpu.pc + 2
                }

        -- 00EE - RET
        OpCode.Ret ->
            case cpu.stack of
                pc :: stack ->
                    Ok
                        { cpu
                            | pc = pc
                            , stack = stack
                        }

                [] ->
                    Err StackUnderflow

        -- 0nnn - SYS addr
        --
        OpCode.Sys _ ->
            Ok cpu

        -- 1nnn - JP addr
        OpCode.Jp addr ->
            Ok { cpu | pc = addr }

        -- 2nnn - CALL addr
        OpCode.Call addr ->
            if List.length cpu.stack >= 16 then
                Err StackOverflow

            else
                Ok
                    { cpu
                        | pc = addr
                        , stack = cpu.pc + 2 :: cpu.stack
                    }

        -- 3xkk - SE Vx, byte
        --
        -- Skip next instruction if Vx = kk.
        OpCode.Se x kk ->
            if Registers.get x cpu.v == kk then
                Ok { cpu | pc = cpu.pc + 4 }

            else
                Ok { cpu | pc = cpu.pc + 2 }

        -- 4xkk - SNE Vx, byte
        --
        -- Skip next instruction if Vx != kk.
        OpCode.Sne x kk ->
            if Registers.get x cpu.v == kk then
                Ok { cpu | pc = cpu.pc + 2 }

            else
                Ok { cpu | pc = cpu.pc + 4 }

        -- 5xy0 - SE Vx, Vy
        --
        -- Skip next instruction if Vx = Vy.
        OpCode.Sev x y ->
            if Registers.get x cpu.v == Registers.get y cpu.v then
                Ok { cpu | pc = cpu.pc + 4 }

            else
                Ok { cpu | pc = cpu.pc + 2 }

        -- 6xkk - LD Vx, byte
        --
        -- Set Vx = nn
        OpCode.Ldv x byte ->
            Ok
                { cpu
                    | v = Registers.set x byte cpu.v
                    , pc = cpu.pc + 2
                }

        -- 9xy0 - SNE Vx, Vy
        --
        -- Skip next instruction if Vx != Vy.
        OpCode.Snev x y ->
            if Registers.get x cpu.v == Registers.get y cpu.v then
                Ok { cpu | pc = cpu.pc + 2 }

            else
                Ok { cpu | pc = cpu.pc + 4 }

        -- 7xkk - ADD Vx, kk
        --
        -- Set Vx = Vx + kk
        OpCode.Add x kk ->
            let
                sum =
                    Registers.get x cpu.v |> Byte.add kk
            in
            Ok
                { cpu
                    | v = Registers.set x sum cpu.v
                    , pc = cpu.pc + 2
                }

        -- 8xy0 - LD Vx, Vy
        --
        -- Set Vx = Vy
        OpCode.Ldvv x y ->
            let
                vy =
                    Registers.get y cpu.v
            in
            Ok
                { cpu
                    | v = Registers.set x vy cpu.v
                    , pc = cpu.pc + 2
                }

        -- 8xy1 - OR Vx, Vy
        --
        -- Set Vx = Vx OR Vy
        OpCode.Or x y ->
            let
                vx =
                    Registers.get x cpu.v

                vy =
                    Registers.get y cpu.v
            in
            Ok
                { cpu
                  -- | v = Registers.set x (Byte.or vx vy) cpu.v
                    | v =
                        cpu.v
                            |> Registers.set x (Byte.or vx vy)
                            -- QUIRK 1: Reset VF to 0
                            |> Registers.set Register.VF (Byte.fromInt 0)
                    , pc = cpu.pc + 2
                }

        -- 8xy2 - AND Vx, Vy
        --
        -- Set Vx = Vx AND Vy
        OpCode.And x y ->
            let
                vx =
                    Registers.get x cpu.v

                vy =
                    Registers.get y cpu.v
            in
            Ok
                { cpu
                  -- | v = Registers.set x (Byte.and vx vy) cpu.v
                    | v =
                        cpu.v
                            |> Registers.set x (Byte.and vx vy)
                            -- QUIRK: Rest VF to 0
                            |> Registers.set Register.VF (Byte.fromInt 0)
                    , pc = cpu.pc + 2
                }

        -- 8xy3 - XOR Vx, Vy
        --
        -- Set Vx = Vx XOR Vy
        OpCode.Xor x y ->
            let
                vx =
                    Registers.get x cpu.v

                vy =
                    Registers.get y cpu.v
            in
            Ok
                { cpu
                  -- | v = Registers.set x (Byte.xor vx vy) cpu.v
                    | v =
                        cpu.v
                            |> Registers.set x (Byte.xor vx vy)
                            -- QUIRK: Rest VF to 0
                            |> Registers.set Register.VF (Byte.fromInt 0)
                    , pc = cpu.pc + 2
                }

        -- 8xy4 - ADD Vx, Vy
        --
        -- Set Vx = Vx + Vy, set VF = carry
        OpCode.Addvv x y ->
            let
                vx =
                    Registers.get x cpu.v

                vy =
                    Registers.get y cpu.v

                ( sum, isCarry ) =
                    Byte.addCarry vx vy
            in
            Ok
                { cpu
                    | v =
                        cpu.v
                            -- Store sum in Vx
                            |> Registers.set x sum
                            -- Store carry in VF
                            |> Registers.set Register.VF (Byte.fromBool isCarry)
                    , pc = cpu.pc + 2
                }

        -- 8xy5 - SUB Vx, Vy
        --
        -- Set Vx = Vx - Vy, set VF = NOT borrow
        OpCode.Sub x y ->
            let
                vx =
                    Registers.get x cpu.v

                vy =
                    Registers.get y cpu.v

                ( diff, isBorrow ) =
                    Byte.subCarry vx vy

                -- For subtraction, VF is a NO-carry flag.
                -- 1 means there was no carry (vx >= vy)
                -- 0 means there was a carry (vx < vy)
                noBorrow =
                    Byte.fromBool (not isBorrow)
            in
            Ok
                { cpu
                    | v =
                        cpu.v
                            -- Write sub result to Vx
                            |> Registers.set x diff
                            -- Write borrow in VF
                            |> Registers.set Register.VF noBorrow
                    , pc = cpu.pc + 2
                }

        -- 8xy6 - SHR Vx {, Vy}
        --
        -- Set Vx = Vy SHR 1
        --
        -- Quirk 3: 8xy6 and 8xye in chip-8 store shifted Vy in Vx.
        -- OpCode.Shr x y ->
        --     let
        --         vy =
        --             Registers.get y cpu.v
        --         lsb =
        --             Byte.lsb vy
        --         halved =
        --             Byte.shiftRight vy
        --     in
        --     Ok
        --         { cpu
        --             | v =
        --                 cpu.v
        --                     -- Write Vx / 2 to Vx
        --                     |> Registers.set x halved
        --                     -- Write Vx's prior LSB to VF
        --                     |> Registers.set Register.VF (Byte.fromBool lsb)
        --             , pc = cpu.pc + 2
        --         }
        OpCode.Shr x _ ->
            let
                vx =
                    Registers.get x cpu.v

                lsb =
                    Byte.lsb vx

                halved =
                    Byte.shiftRight vx
            in
            Ok
                { cpu
                    | v =
                        cpu.v
                            -- Write Vx / 2 to Vx
                            |> Registers.set x halved
                            -- Write Vx's prior LSB to VF
                            |> Registers.set Register.VF (Byte.fromBool lsb)
                    , pc = cpu.pc + 2
                }

        -- 8xy7 - SUBN Vx, Vy
        --
        -- Set Vx = Vy - Vx, set VF = NOT borrow
        OpCode.Subn x y ->
            let
                vx =
                    Registers.get x cpu.v

                vy =
                    Registers.get y cpu.v

                ( diff, isBorrow ) =
                    Byte.subCarry vy vx

                noBorrow =
                    Byte.fromBool (not isBorrow)
            in
            Ok
                { cpu
                    | v =
                        cpu.v
                            -- Write sub result to Vx
                            |> Registers.set x diff
                            -- Write borrow in VF
                            |> Registers.set Register.VF noBorrow
                    , pc = cpu.pc + 2
                }

        -- 8xyE - SHL Vx {, Vy}
        --
        -- Set Vx = Vx SHL 1
        --
        -- Quirk 3: 8xy6 and 8xye in chip-8 store shifted Vy in Vx.
        -- OpCode.Shl x y ->
        --     let
        --         vy =
        --             Registers.get y cpu.v
        --         msb =
        --             Byte.msb vy
        --         doubled =
        --             Byte.shiftLeft vy
        --     in
        --     Ok
        --         { cpu
        --             | v =
        --                 cpu.v
        --                     -- Write Vx * 2 to Vx
        --                     |> Registers.set x doubled
        --                     -- Write Vx's prior MSB to VF
        --                     |> Registers.set Register.VF (Byte.fromBool msb)
        --             , pc = cpu.pc + 2
        --         }
        OpCode.Shl x _ ->
            let
                vx =
                    Registers.get x cpu.v

                msb =
                    Byte.msb vx

                doubled =
                    Byte.shiftLeft vx
            in
            Ok
                { cpu
                    | v =
                        cpu.v
                            -- Write Vx * 2 to Vx
                            |> Registers.set x doubled
                            -- Write Vx's prior MSB to VF
                            |> Registers.set Register.VF (Byte.fromBool msb)
                    , pc = cpu.pc + 2
                }

        -- Annn - LD I, addr
        OpCode.Ldi addr ->
            Ok
                { cpu
                    | i = addr
                    , pc = cpu.pc + 2
                }

        -- Bnnn - JP V0, addr
        --
        -- Jump to location nnn + V0.
        OpCode.Jpv addr ->
            let
                v0 =
                    Registers.get Register.V0 cpu.v
            in
            Ok { cpu | pc = addr + Byte.toInt v0 }

        -- Set Vx = random byte AND kk
        OpCode.Rnd x kk ->
            let
                newSeed =
                    genRandNumber cpu.seed

                randByte =
                    Byte.fromInt newSeed
                        |> Byte.and kk
            in
            Ok
                { cpu
                    | seed = newSeed
                    , v = Registers.set x randByte cpu.v
                    , pc = cpu.pc + 2
                }

        -- Dxyn - DRW Vx, Vy, nibble
        --
        -- Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision
        --
        -- TODO: Return errors from inside helper fn
        -- TODO: Sloppy
        OpCode.Drw x y spriteHeight ->
            let
                width =
                    8

                getSprite : Int -> Cpu -> Byte
                getSprite row cpu_ =
                    -- TODO: Return out of bounds err
                    Array.get (cpu_.i + row) cpu.mem
                        |> Maybe.withDefault (Byte.fromInt 0)

                help : Int -> Int -> Byte -> Cpu -> Cpu
                help row col sprite c =
                    if row < spriteHeight then
                        if col < width then
                            let
                                newCpu =
                                    if Byte.msb sprite then
                                        -- Sprite pixel is ON
                                        let
                                            ( collided, fb ) =
                                                FrameBuffer.setPixel
                                                    (Byte.toInt (Registers.get x cpu.v) + col)
                                                    (Byte.toInt (Registers.get y cpu.v) + row)
                                                    c.frameBuf

                                            newV =
                                                if collided then
                                                    Registers.set Register.VF (Byte.fromInt 1) c.v

                                                else
                                                    c.v
                                        in
                                        { c | frameBuf = fb, v = newV }

                                    else
                                        -- Sprite pixel is OFF
                                        c
                            in
                            help row (col + 1) (Byte.shiftLeft sprite) newCpu

                        else
                            help (row + 1) 0 (getSprite (row + 1) c) c

                    else
                        -- Done
                        c
            in
            -- Reset VF before sending into help
            help 0 0 (getSprite 0 cpu) { cpu | v = Registers.set Register.VF (Byte.fromInt 0) cpu.v }
                |> (\c -> { c | pc = c.pc + 2 })
                |> Ok

        -- Ex9E - SKP Vx
        --
        -- Skip next instruction if key with the value of Vx is pressed.
        OpCode.Skp x ->
            let
                vx =
                    Registers.get x cpu.v
            in
            if Set.member (Byte.toInt vx) cpu.keysDown then
                Ok { cpu | pc = cpu.pc + 4 }

            else
                Ok { cpu | pc = cpu.pc + 2 }

        -- ExA1 - SKNP Vx
        --
        -- Skip next instruction if key with the value of Vx is NOT pressed.
        OpCode.Sknp x ->
            let
                vx =
                    Registers.get x cpu.v
            in
            if Set.member (Byte.toInt vx) cpu.keysDown then
                Ok { cpu | pc = cpu.pc + 2 }

            else
                Ok { cpu | pc = cpu.pc + 4 }

        -- Fx07 - LD Vx, DT
        --
        -- Set Vx = delay timer value.
        OpCode.LoadDelayTimer x ->
            Ok
                { cpu
                    | v = Registers.set x cpu.delayTimer cpu.v
                    , pc = cpu.pc + 2
                }

        -- Fx0A - LD Vx, K
        --
        -- Wait for a key press, store the value of the key in Vx.
        --
        -- PC is not incremented +2 until we receive a keypress.
        OpCode.Ldvk x ->
            -- TODO: Maybe use a key queue of unprocessed keydowns so we can short-circuit
            Ok
                { cpu
                    | awaitingKeyPress = Just x
                }

        -- Fx15 - LD DT, Vx
        --
        -- Set delay timer = Vx.
        OpCode.SetDelayTimer x ->
            let
                vx =
                    Registers.get x cpu.v
            in
            Ok
                { cpu
                    | delayTimer = vx
                    , pc = cpu.pc + 2
                }

        -- Fx18 - LD ST, Vx
        --
        -- Set sound timer = Vx.
        OpCode.SetSoundTimer x ->
            let
                vx =
                    Registers.get x cpu.v
            in
            Ok
                { cpu
                    | soundTimer = vx
                    , pc = cpu.pc + 2
                }

        -- Fx1E - ADD I, Vx
        --
        -- Set I = I + Vx.
        OpCode.Addiv x ->
            let
                vx =
                    Registers.get x cpu.v
            in
            Ok
                { cpu
                    | i = cpu.i + Byte.toInt vx
                    , pc = cpu.pc + 2
                }

        -- Fx29 - LD F, Vx
        --
        -- Set I = location of sprite for digit Vx.
        --
        OpCode.Ldfv x ->
            let
                vx =
                    Registers.get x cpu.v
            in
            Ok
                { cpu
                    | i = Byte.toInt vx * 5
                    , pc = cpu.pc + 2
                }

        -- Fx33 - LD B, Vx
        --
        -- Store BCD representation of Vx in memory locations I, I+1, and I+2.
        --
        -- BCD digits: hundreds, tens, ones
        -- Ex     137:        1,    3,    7
        --
        -- TODO: Return mem out of range errs
        OpCode.Ldbv x ->
            let
                vx =
                    Registers.get x cpu.v
                        |> Byte.toInt

                hundreds =
                    vx // 100

                tens =
                    modBy 10 (vx // 10)

                ones =
                    modBy 10 vx
            in
            Ok
                { cpu
                    | mem =
                        cpu.mem
                            |> Array.set cpu.i (Byte.fromInt hundreds)
                            |> Array.set (cpu.i + 1) (Byte.fromInt tens)
                            |> Array.set (cpu.i + 2) (Byte.fromInt ones)
                    , pc = cpu.pc + 2
                }

        -- Fx55 - LD [I], Vx
        --
        -- Read registers V0 to Vx into memory starting at mem[I] = v0 .. mem[I+x] = vx.
        OpCode.Ldiv x ->
            let
                mem =
                    List.foldl
                        (\reg acc ->
                            let
                                vx =
                                    Registers.get reg cpu.v

                                memIdx =
                                    cpu.i + Register.toInt reg
                            in
                            Array.set memIdx vx acc
                        )
                        cpu.mem
                        (List.take (Register.toInt x + 1) Register.all)
            in
            Ok
                { cpu
                    | mem = mem

                    -- QUIRK 2: Fx55 and Fx65 inc the I register
                    -- , i = cpu.i + 1
                    , pc = cpu.pc + 2
                }

        -- Fx65 - LD Vx, [I]
        -- Read memory into registers V0 to Vx starting at V0 = mem[I] .. vx = mem[I+x].
        OpCode.Ldvi x ->
            let
                v =
                    List.foldl
                        (\reg acc ->
                            let
                                memIdx =
                                    cpu.i + Register.toInt reg

                                memVal =
                                    -- TODO: Handle err
                                    Array.get memIdx cpu.mem
                                        |> Maybe.withDefault (Byte.fromInt 0)
                            in
                            Registers.set reg memVal acc
                        )
                        cpu.v
                        (List.take (Register.toInt x + 1) Register.all)
            in
            Ok
                { cpu
                    | v = v

                    -- QUIRK 2: Fx55 and Fx65 inc the I register
                    -- , i = cpu.i + 1
                    , pc = cpu.pc + 2
                }


{-| List.take for Array.
-}
arrayTake : Int -> Array a -> Array a
arrayTake n =
    Array.slice 0 n
