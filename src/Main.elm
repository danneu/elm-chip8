port module Main exposing (Flags, Model, Msg, Status, main)

import Bitwise
import Browser
import Browser.Dom
import Browser.Events
import Byte
import Cpu exposing (Cpu)
import Dict
import FrameBuffer exposing (FrameBuffer)
import Hex
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Key
import OpCode exposing (OpCode)
import Register
import Registers
import Rom
import Set
import Svg
import Svg.Attributes as SA
import Svg.Keyed
import Task


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


type Status
    = Running
    | Paused


type alias Model =
    { cpu : Cpu
    , seed : Int
    , opHistory : List ( OpCode, Maybe Cpu )
    , status : Status
    , program : List Int
    , allOps : Result String (List ( Int, Maybe OpCode ))
    , followPc : Bool
    }


type alias Flags =
    { seed : Maybe Int
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


initModel : Int -> List Int -> Model
initModel seed program =
    let
        cpu =
            Cpu.fromProgram seed program
    in
    { cpu = cpu
    , seed = seed
    , opHistory = []
    , status = Paused
    , program = program
    , allOps =
        programToOpCodes program
    , followPc = True
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
    ( initModel (Maybe.withDefault 0 flags.seed) Rom.trip8.program
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( initModel (model.seed + 1) model.program
            , Cmd.none
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
                            case ops of
                                [] ->
                                    []

                                op :: rest ->
                                    ( op, Just cpu ) :: List.map (\o -> ( o, Nothing )) rest
                    in
                    ( { model
                        | cpu = cpu
                        , opHistory =
                            List.foldr (::) model.opHistory ops2
                                |> List.take 100
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
            case Dict.get name Rom.all of
                Nothing ->
                    ( model, Cmd.none )

                Just rom ->
                    ( initModel (model.seed + 1) rom.program
                    , Cmd.none
                    )

        Step steps ->
            case Cpu.step steps model.cpu of
                Err _ ->
                    -- TODO: Handle
                    ( model, Cmd.none )

                Ok ( ops, cpu ) ->
                    let
                        ops2 =
                            case ops of
                                [] ->
                                    []

                                op :: rest ->
                                    ( op, Just cpu ) :: List.map (\o -> ( o, Nothing )) rest
                    in
                    ( { model
                        | cpu = cpu
                        , opHistory =
                            List.foldr (::) model.opHistory ops2
                                |> List.take 100
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
        , Html.Attributes.style "border" "1px solid black"
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
            , viewOpCodeHistory model.opHistory
            , viewMemory model.cpu
            ]
        ]


viewRight : Model -> Html Msg
viewRight model =
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


viewKeysDown : Cpu -> Html msg
viewKeysDown cpu =
    div
        [ class "keyboard" ]
        ([ 1, 2, 3, 0x0C, 4, 5, 6, 0x0D, 7, 8, 9, 0x0E, 0x0A, 0, 0x0B, 0x0F ]
            |> List.indexedMap
                (\i v ->
                    let
                        element =
                            span
                                [ classList [ ( "highlight", Set.member v cpu.keysDown ) ]
                                ]
                                [ text (Hex.toString v) ]
                    in
                    if i > 0 && remainderBy 4 i == 0 then
                        [ br [] [], element ]

                    else
                        [ element ]
                )
            |> List.concat
        )


{-| TODO: Replace model.program with model.currentRom or something.
-}
viewRomSelector : List Int -> Html Msg
viewRomSelector currentProgram =
    let
        kindToTitle : Rom.RomKind -> String
        kindToTitle kind =
            case kind of
                Rom.Demo ->
                    "Demos"

                Rom.Test ->
                    "Tests"

                Rom.Game ->
                    "Games"
    in
    label []
        [ text "Load a ROM: "
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
                                    , selected (currentProgram == rom.program)
                                    ]
                                    [ text rom.name ]
                            )
                            (Dict.values Rom.all |> List.filter (\rom -> rom.kind == kind))
                        )
                )
                [ Rom.Demo, Rom.Test, Rom.Game ]
            )
        ]


viewMid : Model -> Html Msg
viewMid model =
    div
        [ class "mid" ]
        [ viewRomSelector model.program
        , br [] []
        , viewFrameBuffer model.cpu.frameBuf
        , br [] []
        , viewKeysDown model.cpu
        , br [] []
        , button [ onClick (ChangeStatus Running), disabled (model.status == Running) ] [ text "Run" ]
        , button [ onClick (ChangeStatus Paused), disabled (model.status == Paused) ] [ text "Pause" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , case model.status of
            Running ->
                span [ style "position" "relative" ]
                    [ span [ style "position" "absolute", class "rotating" ] [ text "⌛" ]
                    , span [ style "margin-left" "1em" ] [ text " Running..." ]
                    ]

            _ ->
                text ""
        , br [] []
        , text ("Steps: " ++ String.fromInt model.cpu.steps)
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
        , label []
            [ input
                [ type_ "range"
                , Html.Attributes.min "60"
                , Html.Attributes.max "1000"
                , step "100"
                , value (String.fromFloat model.cpu.opsPerSecond)
                , Html.Events.onInput (\s -> String.toFloat s |> Maybe.withDefault model.cpu.opsPerSecond |> ChangeCpuSpeed)
                ]
                []
            , text
                (" Cpu speed: "
                    ++ (if model.cpu.opsPerSecond > 800 then
                            "max"

                        else
                            String.fromFloat model.cpu.opsPerSecond ++ " ops/sec"
                       )
                )
            ]
        , case model.cpu.awaitingKeyPress of
            Nothing ->
                text ""

            Just x ->
                p [] [ text ("Awaiting key press for register V" ++ Hex.toString (Register.toInt x) ++ " ") ]
        ]


spacer =
    div [ class "spacer" ] []


view : Model -> Browser.Document Msg
view model =
    { title = "elm-chip8"
    , body =
        [ div
            [ class "container" ]
            [ viewLeft model
            , spacer
            , viewMid model
            , spacer
            , viewRight model
            ]

        -- Show all opcodes
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
