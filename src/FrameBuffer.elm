module FrameBuffer exposing (FrameBuffer, empty, setPixel, toList)

{-| TODO: Replace with bytes where bits encode whether pixel is on/off.
-}

import Array exposing (Array)


cols : Int
cols =
    64


rows : Int
rows =
    32


{-| 64x32 bit frame buffer.

It's (x, y) addressable memory that designates whether a pixel is on or off.

Impl: It's an array of columns.

-}
type FrameBuffer
    = FrameBuffer (Array Bool)


xor : Bool -> Bool -> Bool
xor a b =
    not (a == b)


empty : FrameBuffer
empty =
    FrameBuffer (Array.initialize (cols * rows) (\_ -> False))


coordsToIndex : Int -> Int -> Int
coordsToIndex x_ y_ =
    let
        x =
            modBy cols x_

        y =
            modBy rows y_
    in
    x + (y * cols)


indexToCoords : Int -> ( Int, Int )
indexToCoords idx =
    let
        y =
            idx // cols

        x =
            remainderBy cols idx
    in
    ( x, y )


setPixel : Int -> Int -> FrameBuffer -> ( Bool, FrameBuffer )
setPixel x y (FrameBuffer array) =
    let
        idx =
            coordsToIndex x y

        prev =
            Array.get idx array
                |> Maybe.withDefault False

        newPixel =
            xor prev True
    in
    ( not newPixel
    , FrameBuffer (Array.set idx newPixel array)
    )


toList : FrameBuffer -> List ( ( Int, Int ), Bool )
toList (FrameBuffer array) =
    Array.indexedMap
        (\i v ->
            ( indexToCoords i, v )
        )
        array
        |> Array.toList
