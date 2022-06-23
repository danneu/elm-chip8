module Register exposing (Register(..), all, fromInt, toInt)


type Register
    = V0
    | V1
    | V2
    | V3
    | V4
    | V5
    | V6
    | V7
    | V8
    | V9
    | VA
    | VB
    | VC
    | VD
    | VE
    | VF


all : List Register
all =
    [ V0
    , V1
    , V2
    , V3
    , V4
    , V5
    , V6
    , V7
    , V8
    , V9
    , VA
    , VB
    , VC
    , VD
    , VE
    , VF
    ]


fromInt : Int -> Maybe Register
fromInt index =
    case index of
        0 ->
            Just V0

        1 ->
            Just V1

        2 ->
            Just V2

        3 ->
            Just V3

        4 ->
            Just V4

        5 ->
            Just V5

        6 ->
            Just V6

        7 ->
            Just V7

        8 ->
            Just V8

        9 ->
            Just V9

        0x0A ->
            Just VA

        0x0B ->
            Just VB

        0x0C ->
            Just VC

        0x0D ->
            Just VD

        0x0E ->
            Just VE

        0x0F ->
            Just VF

        _ ->
            Nothing


toInt : Register -> Int
toInt register =
    case register of
        V0 ->
            0

        V1 ->
            1

        V2 ->
            2

        V3 ->
            3

        V4 ->
            4

        V5 ->
            5

        V6 ->
            6

        V7 ->
            7

        V8 ->
            8

        V9 ->
            9

        VA ->
            0x0A

        VB ->
            0x0B

        VC ->
            0x0C

        VD ->
            0x0D

        VE ->
            0x0E

        VF ->
            0x0F
