module Key exposing (Key(..), qwertyMap, toInt)

import Dict exposing (Dict)


type Key
    = Key0
    | Key1
    | Key2
    | Key3
    | Key4
    | Key5
    | Key6
    | Key7
    | Key8
    | Key9
    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF


{-| Map QWERTY keyboard to Chip8 keys using browser `event.code` values.

    QWERTY  -> CHIP-8
    -------    -------
    1 2 3 4    1 2 3 c
    q w e r    4 5 6 d
    a s d f    7 8 9 e
    z x c v    a 0 b f

    document.addEventListener('keydown', (e) => e.code)

-}
qwertyMap : Dict String Key
qwertyMap =
    Dict.fromList
        [ ( "Digit1", Key1 )
        , ( "Digit2", Key2 )
        , ( "Digit3", Key3 )
        , ( "Digit4", KeyC )

        --
        , ( "KeyQ", Key4 )
        , ( "KeyW", Key5 )
        , ( "KeyE", Key6 )
        , ( "KeyR", KeyD )

        --
        , ( "KeyA", Key7 )
        , ( "KeyS", Key8 )
        , ( "KeyD", Key9 )
        , ( "KeyF", KeyE )

        --
        , ( "KeyZ", KeyA )
        , ( "KeyX", Key0 )
        , ( "KeyC", KeyB )
        , ( "KeyV", KeyF )
        ]


toInt : Key -> Int
toInt key =
    case key of
        Key0 ->
            0

        Key1 ->
            1

        Key2 ->
            2

        Key3 ->
            3

        Key4 ->
            4

        Key5 ->
            5

        Key6 ->
            6

        Key7 ->
            7

        Key8 ->
            8

        Key9 ->
            9

        KeyA ->
            0x0A

        KeyB ->
            0x0B

        KeyC ->
            0x0C

        KeyD ->
            0x0D

        KeyE ->
            0x0E

        KeyF ->
            0x0F



-- fromInt : Int -> Maybe Key
-- fromInt n =
--     case n of
--         0 ->
--             Just Key0
--         1 ->
--             Just Key1
--         2 ->
--             Just Key2
--         3 ->
--             Just Key3
--         4 ->
--             Just Key4
--         5 ->
--             Just Key5
--         6 ->
--             Just Key6
--         7 ->
--             Just Key7
--         8 ->
--             Just Key8
--         9 ->
--             Just Key9
--         0x0A ->
--             Just KeyA
--         0x0B ->
--             Just KeyB
--         0x0C ->
--             Just KeyC
--         0x0D ->
--             Just KeyD
--         0x0E ->
--             Just KeyE
--         0x0F ->
--             Just KeyF
--         _ ->
--             Nothing
