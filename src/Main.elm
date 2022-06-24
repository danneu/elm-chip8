port module Main exposing (Flags, Model, Msg, Status, main)

import Bitwise
import Browser
import Browser.Dom
import Browser.Events
import Byte
import Bytes exposing (Bytes)
import Bytes.Decode as B
import Cpu exposing (Cpu)
import Dict
import File
import File.Select
import FrameBuffer exposing (FrameBuffer)
import Hex
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Json.Decode as D
import Json.Encode as E
import Key
import OpCode exposing (OpCode)
import Register
import Registers
import SampleRom
import Set
import Svg
import Svg.Attributes as SA
import Svg.Keyed
import Task


port storeConfig : E.Value -> Cmd msg


port scrollToId : String -> Cmd msg


port playAudio : Bool -> Cmd msg


type Msg
    = NoOp
    | Tick Float
    | Step Int
    | KeyDown Key.Key
    | KeyUp Key.Key
    | ClearKeys
    | ChangeStatus Status
    | Reset
    | RomSelected String
    | ChangeFollowPc Bool
    | ChangeCpuSpeed Float
    | RevertToCpu Int Cpu
    | ChangeDebug DebugOptions
    | RequestUserFile
    | UserFileSelected File.File
    | UserFileLoaded Bytes


type Status
    = Running
    | Paused


type alias DebugOptions =
    { showDebug : Bool

    -- We keep the state of suboptions so that we can toggle debug without losing state
    , showHistory : Bool
    , showProgram : Bool
    }


defaultDebug : DebugOptions
defaultDebug =
    { showDebug = True
    , showHistory = True
    , showProgram = True
    }


defaultConfig : Config
defaultConfig =
    { debug = defaultDebug
    , rom = Nothing
    }


type Rom
    = SampleRom String (List Int)
    | CustomRom (List Int)


getProgram : Rom -> List Int
getProgram r =
    case r of
        SampleRom _ x ->
            x

        CustomRom x ->
            x


type alias Model =
    { cpu : Cpu
    , seed : Int
    , opHistory : List ( OpCode, Maybe Cpu )
    , status : Status

    -- , program : List Int
    -- , romKey : Maybe String
    -- , rom : Rom.Rom
    , rom : Rom
    , allOps : Result String (List ( Int, Maybe OpCode ))
    , followPc : Bool
    , debug : DebugOptions
    }


type alias Flags =
    { seed : Maybe Int
    , config : Maybe E.Value
    }


programToOpCodes : List Int -> Result String (List ( Int, Maybe OpCode ))
programToOpCodes bytes =
    let
        help bs acc =
            case bs of
                [] ->
                    Ok (List.reverse acc)

                _ :: [] ->
                    -- Odd numer of bytes
                    -- Err "odd"
                    --
                    -- TODO: Stars program jumps to odd PC, so my attempt at decoding opcodes at even addrs is unreliable
                    --       but it seems to work for most roms so far. For now let's no-op.
                    Ok (List.reverse acc)

                hi :: lo :: rest ->
                    let
                        code =
                            Bitwise.or lo (Bitwise.shiftLeftBy 8 hi)
                    in
                    case OpCode.fromInt code of
                        Nothing ->
                            help rest (( code, Nothing ) :: acc)

                        Just op ->
                            help rest (( code, Just op ) :: acc)
    in
    help bytes []


initModel : Int -> DebugOptions -> Rom -> Model
initModel seed debug rom =
    let
        cpu =
            Cpu.fromProgram seed (getProgram rom)
    in
    { cpu = cpu
    , seed = seed
    , opHistory = []
    , status = Paused
    , rom = rom
    , allOps =
        programToOpCodes (getProgram rom)
    , followPc = True
    , debug = debug

    -- , romKey = Just rom.name
    }


toProgram : List Int -> List Int
toProgram opcodes =
    List.concatMap
        (\code ->
            let
                hi =
                    Bitwise.shiftRightBy 8 code

                lo =
                    Bitwise.and 0xFF code
            in
            [ hi, lo ]
        )
        opcodes


{-| A tiny program that renders an '8' char at the bottom of the viewport to see that it wraps.
-}
spriteClipTest : List Int
spriteClipTest =
    toProgram
        [ 0x6008 -- 6xnn: Set V0 to loc of sprite for char 0x7 (V0 = 0x8)
        , 0xF029 -- Fx29: Set I to loc of sprite for char in Vx (V0 )

        -- set (x, y)
        , 0x610A -- 6xnn: Set Vx = byte nn (V1 = 0xA)
        , 0x621E -- 6xnn: Set Vx = byte nn (V2 = 0x1E = 30)
        , 0xD125 -- Dxyn
        ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        config =
            flags.config
                |> Maybe.andThen (\v -> D.decodeValue decodeConfig v |> Result.toMaybe)
                |> Maybe.withDefault defaultConfig

        rom =
            config.rom
                |> Maybe.withDefault (SampleRom SampleRom.trip8.name SampleRom.trip8.program)
    in
    ( initModel (Maybe.withDefault 0 flags.seed) config.debug rom
    , Cmd.none
    )


viewMemory : Cpu -> Html Msg
viewMemory _ =
    -- ol
    --     [ start 0 ]
    --     (List.indexedMap
    --         (\i n ->
    --             let
    --                 addr =
    --                     "0x" ++ String.padLeft 2 '0' (Hex.toString i)
    --                 value =
    --                     "0x" ++ String.padLeft 2 '0' (Hex.toString n)
    --             in
    --             li []
    --                 [ text (addr ++ ": " ++ value) ]
    --         )
    --         (Array.toList cpu.mem)
    --     )
    text ""



-- type alias Model =
--     { cpu : Cpu
--     , seed : Int
--     , opHistory : List ( OpCode, Maybe Cpu )
--     , status : Status
--     , program : List Int
--     , allOps : Result String (List ( Int, Maybe OpCode ))
--     , followPc : Bool
--     , debug : DebugOptions
--     }


{-| A subset of the model that we save to localStorage.
-}
type alias Config =
    { debug : DebugOptions
    , rom : Maybe Rom
    }


encodeConfig : Model -> E.Value
encodeConfig ({ debug } as m) =
    let
        encodeRom : Rom -> E.Value
        encodeRom r =
            case r of
                CustomRom data ->
                    E.list E.int data

                SampleRom s bs ->
                    E.list identity [ E.string s, E.list E.int bs ]
    in
    E.object
        [ ( "debug"
          , E.object
                [ ( "showDebug", E.bool debug.showDebug )
                , ( "showHistory", E.bool debug.showHistory )
                , ( "showProgram", E.bool debug.showProgram )
                ]
          )
        , ( "rom", encodeRom m.rom )
        ]


decodeConfig : D.Decoder Config
decodeConfig =
    D.map2 Config
        (D.field "debug"
            (D.map3 DebugOptions
                (D.field "showDebug" D.bool)
                (D.field "showHistory" D.bool)
                (D.field "showProgram" D.bool)
            )
        )
        -- rom: [1,2,3,...]
        -- rom: {kind: "Game", name: "...", program: [1,2,3], desc: """}
        (D.field "rom"
            (D.oneOf
                [ D.list D.int |> D.map (CustomRom >> Just)
                , D.map2 SampleRom
                    (D.index 0 D.string)
                    (D.index 1 (D.list D.int))
                    |> D.map Just
                , D.succeed Nothing
                ]
            )
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( initModel (model.seed + 1) model.debug model.rom
            , Cmd.none
            )

        ChangeDebug d ->
            let
                m =
                    { model | debug = d }
            in
            ( m
            , storeConfig (encodeConfig m)
            )

        -- TODO: Move tick execution into Cpu. `step` should use it. ie.g. User should not have to be the one decr'ing timers.
        -- Tick delta ->
        --     let
        --         instrCount =
        --             Basics.max 1 (floor ((delta / 1000) * instrPerSecond))
        --     in
        --     case Cpu.step instrCount model.cpu of
        --         Err _ ->
        --             -- TODO: Handle
        --             ( model, Cmd.none )
        --         Ok ( ops, cpu ) ->
        --             ( { model
        --                 | cpu =
        --                     cpu
        --                         |> (\c ->
        --                                 { c
        --                                     | delayTimer =
        --                                         if Byte.isZero c.delayTimer then
        --                                             c.delayTimer
        --                                         else
        --                                             Byte.dec c.delayTimer
        --                                 }
        --                            )
        --                 , ops =
        --                     List.foldr (::) model.ops ops
        --                         |> List.take 100
        --               }
        --             , if model.followPc then
        --                 scrollToId ("program-explorer-" ++ String.fromInt cpu.pc)
        --               else
        --                 Cmd.none
        --             )
        Tick delta ->
            case Cpu.tick delta model.cpu of
                Err _ ->
                    -- TODO: Handle
                    ( model, Cmd.none )

                Ok ( ops, cpu ) ->
                    let
                        ops2 =
                            if model.debug.showHistory then
                                case ops of
                                    [] ->
                                        []

                                    op :: rest ->
                                        ( op, Just cpu ) :: List.map (\o -> ( o, Nothing )) rest

                            else
                                []
                    in
                    ( { model
                        | cpu = cpu
                        , opHistory =
                            if model.debug.showHistory then
                                List.foldr (::) model.opHistory ops2
                                    |> List.take 100

                            else
                                []
                      }
                    , Cmd.batch
                        [ if model.followPc then
                            scrollToId ("program-explorer-" ++ String.fromInt cpu.pc)

                          else
                            Cmd.none
                        , if Byte.isZero cpu.soundTimer then
                            playAudio False

                          else
                            playAudio True
                        ]
                    )

        ChangeStatus status ->
            ( { model | status = status }
            , Cmd.none
            )

        KeyDown key ->
            ( { model | cpu = Cpu.keyDown key model.cpu }
            , Cmd.none
            )

        KeyUp key ->
            ( { model | cpu = Cpu.keyUp key model.cpu }
            , Cmd.none
            )

        ClearKeys ->
            ( { model | cpu = Cpu.clearKeys model.cpu }
            , Cmd.none
            )

        RomSelected name ->
            case Dict.get name SampleRom.all of
                Nothing ->
                    ( model, Cmd.none )

                Just rom ->
                    let
                        m =
                            initModel (model.seed + 1) model.debug (SampleRom rom.name rom.program)
                    in
                    ( m
                    , storeConfig (encodeConfig m)
                    )

        Step steps ->
            case Cpu.step steps model.cpu of
                Err _ ->
                    -- TODO: Handle
                    ( model, Cmd.none )

                Ok ( ops, cpu ) ->
                    let
                        ops2 =
                            if model.debug.showHistory then
                                case ops of
                                    [] ->
                                        []

                                    op :: rest ->
                                        ( op, Just cpu ) :: List.map (\o -> ( o, Nothing )) rest

                            else
                                []
                    in
                    ( { model
                        | cpu = cpu
                        , opHistory =
                            if model.debug.showHistory then
                                List.foldr (::) model.opHistory ops2
                                    |> List.take 100

                            else
                                []
                      }
                      -- , scrollToPc cpu.pc
                    , Cmd.batch
                        [ if model.followPc then
                            scrollToId ("program-explorer-" ++ String.fromInt cpu.pc)

                          else
                            Cmd.none
                        , if Byte.isZero cpu.soundTimer then
                            playAudio False

                          else
                            playAudio True
                        ]
                    )

        ChangeFollowPc bool ->
            ( { model | followPc = bool }
            , Cmd.none
            )

        ChangeCpuSpeed speed ->
            ( { model | cpu = Cpu.withSpeed speed model.cpu }
            , Cmd.none
            )

        -- distance is the number of ops new cpu is from prev cpu.
        -- It's used to trim op history sidebar.
        RevertToCpu distance cpu ->
            ( { model
                | cpu = cpu
                , opHistory = List.drop distance model.opHistory
              }
            , Cmd.none
            )

        RequestUserFile ->
            ( model
            , File.Select.file [ ".ch8" ] UserFileSelected
            )

        UserFileSelected file ->
            ( model
            , Task.perform UserFileLoaded (File.toBytes file)
            )

        UserFileLoaded bytes_ ->
            let
                len =
                    Bytes.width bytes_

                decoder =
                    B.loop ( 0, [] ) <|
                        \( n, acc ) ->
                            if n >= len then
                                B.succeed (B.Done (List.reverse acc))

                            else
                                B.unsignedInt8
                                    |> B.andThen
                                        (\b ->
                                            B.Loop ( n + 1, b :: acc )
                                                |> B.succeed
                                        )

                rom =
                    B.decode decoder bytes_
                        |> Maybe.map CustomRom
                        -- TODO: Default rom here doesn't make sense
                        |> Maybe.withDefault (CustomRom [])

                m =
                    initModel model.seed model.debug rom
            in
            ( m
            , storeConfig (encodeConfig m)
            )


scrollToPc : Int -> Cmd Msg
scrollToPc pc =
    let
        elementId =
            "program-explorer-"
                ++ String.fromInt pc
    in
    Task.map2 Tuple.pair
        (Browser.Dom.getViewportOf "program-explorer")
        (Browser.Dom.getElement elementId)
        |> Task.andThen
            (\( _, elementInfo ) ->
                Browser.Dom.setViewportOf "right" 0 (elementInfo.element.y - 20)
            )
        |> Task.attempt (\_ -> NoOp)


showNumber : Int -> String
showNumber n =
    "Ox" ++ String.padLeft 2 '0' (Hex.toString n) ++ " (" ++ String.fromInt n ++ ")"


viewFrameBuffer : FrameBuffer -> Html msg
viewFrameBuffer fb =
    Svg.Keyed.node "svg"
        [ SA.viewBox "0 0 64 32"
        , SA.height "200px"

        -- , Html.Attributes.style "border" "1px solid black"
        ]
        (FrameBuffer.toList fb
            |> List.indexedMap
                (\i ( ( x, y ), isOn ) ->
                    ( String.fromInt i
                    , Svg.rect
                        [ SA.x <| String.fromInt x ++ "px"
                        , SA.y <| String.fromInt y ++ "px"
                        , SA.width "1px"
                        , SA.height "1px"
                        , SA.fill
                            (if isOn then
                                "black"

                             else
                                "none"
                            )
                        , SA.stroke "black"
                        , SA.strokeWidth "0.025"
                        ]
                        []
                    )
                )
        )


to0xHexString : Int -> String
to0xHexString n =
    "0x" ++ String.padLeft 2 '0' (Hex.toString n)


find : (a -> Bool) -> List a -> Maybe a
find pred xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            if pred x then
                Just x

            else
                find pred rest


viewCpu : List ( OpCode, Maybe Cpu ) -> Cpu -> Html msg
viewCpu history cpu =
    let
        prev =
            List.drop 1 history
                |> find (\( _, mc ) -> mc /= Nothing)
                |> Maybe.map Tuple.second
                |> Maybe.andThen identity
                |> Maybe.withDefault cpu
    in
    div
        []
        [ h2 [] [ text "CPU state" ]
        , ul
            [ class "cpu-state" ]
            (List.concat
                [ [ li [] [ text ("PC: " ++ showNumber cpu.pc) ]

                  --   , li [] [ text ("DT: " ++ String.fromInt (Byte.toInt cpu.delayTimer)) ]
                  , li [ classList [ ( "highlight", prev.delayTimer /= cpu.delayTimer ) ] ] [ text ("DT: " ++ showNumber (Byte.toInt cpu.delayTimer)) ]
                  , li [ classList [ ( "highlight", prev.soundTimer /= cpu.soundTimer ) ] ] [ text ("ST: " ++ showNumber (Byte.toInt cpu.soundTimer)) ]
                  , li [ classList [ ( "highlight", prev.i /= cpu.i ) ] ] [ text ("_I: " ++ showNumber cpu.i) ]
                  ]
                , List.indexedMap
                    (\i reg ->
                        let
                            vx =
                                Registers.get reg cpu.v
                        in
                        li
                            [ classList [ ( "highlight", Registers.get reg prev.v /= vx ) ] ]
                            [ text
                                ("V"
                                    ++ Hex.toString i
                                    ++ ": "
                                    -- Hex value
                                    ++ ("0x" ++ String.padLeft 2 '0' (Hex.toString (Byte.toInt vx)))
                                    -- Dec value
                                    ++ " ("
                                    ++ String.fromInt (Byte.toInt vx)
                                    ++ ")"
                                )
                            ]
                    )
                    Register.all
                ]
            )
        ]


viewStack : Cpu -> Html msg
viewStack cpu =
    div []
        [ h2 [] [ text "Stack:" ]
        , if List.isEmpty cpu.stack then
            text "(empty)"

          else
            ul
                []
                (List.map
                    (\addr ->
                        li
                            []
                            [ text (to0xHexString addr) ]
                    )
                    cpu.stack
                )
        ]


viewOpCodeHistory : List ( OpCode, Maybe Cpu ) -> Html Msg
viewOpCodeHistory ops =
    div
        []
        [ h3 [] [ text "Execution history:" ]
        , if List.isEmpty ops then
            text "(empty)"

          else
            ul
                []
                (List.indexedMap
                    (\i ( op, maybeCpu ) ->
                        li
                            []
                            [ text <| OpCode.toString op
                            , case ( i, maybeCpu ) of
                                ( _, Nothing ) ->
                                    text ""

                                ( 0, _ ) ->
                                    text ""

                                ( _, Just cpu ) ->
                                    button [ onClick (RevertToCpu i cpu) ]
                                        [ text <| "revert " ]
                            ]
                    )
                    -- (List.take 10 ops)
                    ops
                )
        ]


viewLeft : Model -> Html Msg
viewLeft model =
    div
        [ class "left" ]
        [ div [ class "scroller" ]
            [ viewCpu model.opHistory model.cpu
            , hr [] []
            , viewStack model.cpu
            , hr [] []
            , if model.debug.showHistory then
                viewOpCodeHistory model.opHistory

              else
                text ""
            , viewMemory model.cpu
            ]
        ]


viewRight : Model -> Html Msg
viewRight model =
    if not model.debug.showProgram then
        text ""

    else
        div
            [ class "right"
            , id "right" -- for Brower.Dom viewport
            ]
            [ label
                []
                [ text "Follow PC: "
                , input
                    [ type_ "checkbox"
                    , Html.Events.onCheck ChangeFollowPc
                    , checked model.followPc
                    ]
                    []
                ]
            , case model.allOps of
                Err _ ->
                    text "TODO: Handle error"

                Ok allOps ->
                    let
                        hex n =
                            "0x" ++ String.padLeft 4 '0' (Hex.toString n)
                    in
                    ol
                        [ start 0x0200
                        , id "program-explorer"
                        , style "padding-bottom" "50vh"
                        ]
                        (List.indexedMap
                            (\i ( int, maybeOp ) ->
                                -- Index to PC -> 0x200 + index * 2
                                -- PC to Index -> PC - 0x200 // 2
                                li
                                    -- [ value (String.fromInt <| (0x0200 + (i * 2)))
                                    [ id ("program-explorer-" ++ String.fromInt (0x0200 + (i * 2)))
                                    , classList [ ( "highlight", (model.cpu.pc - 0x0200) // 2 == i ) ]
                                    ]
                                    [ -- Item hex index
                                      span
                                        [ title (String.fromInt (0x0200 + (i * 2)))
                                        ]
                                        [ text ("0x" ++ Hex.toString (0x0200 + (i * 2)) ++ " ") ]

                                    -- Item hex value
                                    , text (hex int)
                                    , text " "
                                    , case maybeOp of
                                        Nothing ->
                                            text "--"

                                        Just op ->
                                            code [ style "white-space" "pre" ] [ text (OpCode.toString op) ]

                                    -- case maybeOp of
                                    -- Nothing ->
                                    ]
                            )
                            allOps
                        )
            ]


viewKeysDown : Cpu -> Html Msg
viewKeysDown cpu =
    div
        [ class "keyboard" ]
        ([ [ 1, 2, 3, 0x0C ], [ 4, 5, 6, 0x0D ], [ 7, 8, 9, 0x0E ], [ 0x0A, 0, 0x0B, 0x0F ] ]
            |> List.map
                (\vs ->
                    div
                        [ class "row"
                        ]
                        (List.map
                            (\v ->
                                let
                                    key =
                                        Key.fromInt v
                                            |> Maybe.withDefault Key.Key0
                                in
                                button
                                    [ classList [ ( "highlight", Set.member v cpu.keysDown ) ]
                                    , Html.Events.onMouseDown (KeyDown key)
                                    , Html.Events.onMouseUp (KeyUp key)
                                    , Html.Events.onMouseLeave (KeyUp key)
                                    ]
                                    [ text (String.toUpper <| Hex.toString v) ]
                            )
                            vs
                        )
                )
        )


viewRomSelector : Rom -> Html Msg
viewRomSelector currentRom =
    let
        kindToTitle : SampleRom.RomKind -> String
        kindToTitle kind =
            case kind of
                SampleRom.Demo ->
                    "Demos"

                SampleRom.Test ->
                    "Tests"

                SampleRom.Game ->
                    "Games"

        selectedRomIsCustom =
            case currentRom of
                SampleRom _ _ ->
                    False

                CustomRom _ ->
                    True
    in
    div
        [ style "margin-top" "1em"
        , class "rom-selector"
        ]
        [ label
            [ style "display" "block"
            , classList [ ( "highlight", not selectedRomIsCustom ) ]
            , class "rom-input"
            ]
            [ text "Built-in ROMs: "
            , select
                [ Html.Events.onInput RomSelected ]
                (List.map
                    (\kind ->
                        optgroup
                            [ attribute "label" (kindToTitle kind) ]
                            (List.map
                                (\rom ->
                                    option
                                        [ value rom.name
                                        , selected <|
                                            case currentRom of
                                                SampleRom key _ ->
                                                    key == rom.name

                                                _ ->
                                                    False
                                        ]
                                        [ text rom.name ]
                                )
                                (Dict.values SampleRom.all |> List.filter (\rom -> rom.kind == kind))
                            )
                    )
                    [ SampleRom.Demo, SampleRom.Test, SampleRom.Game ]
                )
            ]
        , div
            [ classList [ ( "highlight", selectedRomIsCustom ) ]
            , style "display" "block"
            , class "rom-input"
            ]
            [ text "Load your own ROM (.ch8) file: "
            , button [ onClick RequestUserFile, type_ "button" ] [ text "Choose local file" ]
            ]
        , br [] []
        , p [ style "text-align" "center" ]
            [ case currentRom of
                SampleRom _ program ->
                    text ("ROM loaded from samples. (" ++ String.fromInt (List.length program) ++ " bytes)")

                CustomRom program ->
                    text ("ROM loaded from user file. (" ++ String.fromInt (List.length program) ++ " bytes)")
            ]
        ]


viewDebugOptions : DebugOptions -> Html Msg
viewDebugOptions d =
    div
        [ class "debug-options"
        ]
        [ hr [] []
        , div
            [ style "text-align" "center" ]
            [ label
                []
                [ text "Show debug"
                , input
                    [ type_ "checkbox"
                    , checked d.showDebug
                    , onCheck (\v -> ChangeDebug { d | showDebug = v })
                    ]
                    []
                ]
            ]
        , div
            [ style "display" "flex"
            , style "justify-content" "space-evenly"
            ]
            [ div
                []
                [ label
                    [ classList [ ( "disabled", not d.showDebug ) ] ]
                    [ text "Show history"
                    , input
                        [ type_ "checkbox"
                        , disabled (not d.showDebug)
                        , checked d.showHistory
                        , onCheck (\v -> ChangeDebug { d | showHistory = v })
                        ]
                        []
                    ]
                ]
            , div
                []
                [ label
                    [ classList [ ( "disabled", not d.showDebug ) ] ]
                    [ text "Show program"
                    , input
                        [ type_ "checkbox"
                        , disabled (not d.showDebug)
                        , checked d.showProgram
                        , onCheck (\v -> ChangeDebug { d | showProgram = v })
                        ]
                        []
                    ]
                ]
            ]
        ]


viewMid : Model -> Html Msg
viewMid model =
    div
        [ class "mid" ]
        [ div
            [ class "scroller" ]
            [ div
                [ style "max-width" "fit-content"
                , style "margin" "0 auto"
                ]
                [ viewRomSelector model.rom
                , br [] []
                , viewFrameBuffer model.cpu.frameBuf
                , br [] []
                , case model.cpu.awaitingKeyPress of
                    Nothing ->
                        -- Lazy way of avoiding the awaiting-key banner from pushing content down
                        p [ style "visibility" "hidden" ] [ text "-" ]

                    Just x ->
                        p
                            [ class "awaiting"
                            ]
                            [ text ("⚠️ Awaiting key press for register V" ++ Hex.toString (Register.toInt x) ++ " ") ]
                , div
                    [ class "playback-controls" ]
                    [ case model.status of
                        Running ->
                            button [ onClick (ChangeStatus Paused), disabled (model.status == Paused) ] [ text "Pause" ]

                        Paused ->
                            button
                                [ onClick (ChangeStatus Running)
                                , disabled (model.status == Running)

                                -- , classList
                                --     [ ( "pulse-blue"
                                --       , model.status == Paused
                                --       )
                                --     ]
                                ]
                                [ text "Run" ]
                    , button [ onClick Reset ] [ text "Reload" ]
                    ]
                , br [] []
                , div
                    [ style "display" "flex"
                    , style "justify-content" "space-evenly"
                    ]
                    [ viewKeysDown model.cpu
                    , div
                        [ --style "flex-grow" "2"
                          style "padding-left" "1em"
                        ]
                        [ text ("Steps: " ++ String.fromInt model.cpu.steps)
                        , case model.status of
                            Running ->
                                span
                                    [ style "display" "inline-block"
                                    , class "rotating"
                                    , style "margin-left" ".5em"
                                    ]
                                    [ text "⌛"
                                    ]

                            _ ->
                                text ""
                        , br [] []
                        , button
                            [ onClick (Step 1)
                            , tabindex 1
                            ]
                            [ text "Step +1" ]
                        , button
                            [ onClick (Step 10)
                            , tabindex 1
                            ]
                            [ text "Step +10" ]
                        , button
                            [ onClick (Step 100)
                            , tabindex 1
                            ]
                            [ text "Step +100" ]
                        , br [] []
                        , br [] []
                        , label []
                            [ text
                                (" Cpu speed: "
                                    ++ (if model.cpu.opsPerSecond > 800 then
                                            "max"

                                        else
                                            String.fromFloat model.cpu.opsPerSecond ++ " ops/sec"
                                       )
                                )
                            , input
                                [ type_ "range"
                                , Html.Attributes.min "60"
                                , Html.Attributes.max "1000"
                                , style "display" "block"
                                , style "width" "100%"
                                , step "100"
                                , value (String.fromFloat model.cpu.opsPerSecond)
                                , Html.Events.onInput (\s -> String.toFloat s |> Maybe.withDefault model.cpu.opsPerSecond |> ChangeCpuSpeed)
                                ]
                                []
                            ]
                        ]
                    ]
                , viewDebugOptions model.debug
                ]
            ]
        ]


spacer : Html msg
spacer =
    div [ class "spacer" ] []


view : Model -> Browser.Document Msg
view model =
    { title = "elm-chip8"
    , body =
        [ div
            [ class "container" ]
            [ if model.debug.showDebug then
                viewLeft model

              else
                text ""

            -- , spacer
            , viewMid model

            -- , spacer
            , if model.debug.showDebug then
                viewRight model

              else
                text ""
            ]
        ]
    }


decodeKey : D.Decoder Key.Key
decodeKey =
    D.field "code" D.string
        |> D.andThen
            (\code ->
                case Dict.get code Key.qwertyMap of
                    Nothing ->
                        D.fail "uninteresting key"

                    Just key ->
                        D.succeed key
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown
            (decodeKey |> D.map KeyDown)
        , Browser.Events.onKeyUp
            (decodeKey |> D.map KeyUp)
        , Browser.Events.onVisibilityChange (\_ -> ClearKeys)
        , Browser.Events.onVisibilityChange
            (\vis ->
                case vis of
                    Browser.Events.Hidden ->
                        ChangeStatus Paused

                    _ ->
                        NoOp
            )
        , case model.status of
            Running ->
                Browser.Events.onAnimationFrameDelta Tick

            _ ->
                Sub.none
        ]


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
