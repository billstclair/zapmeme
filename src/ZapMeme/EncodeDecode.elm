---------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON encoders and decoders.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ZapMeme.EncodeDecode exposing
    ( decodeMeme
    , decodeSavedModel
    , encodeMeme
    , encodeSavedModel
    , memeDecoder
    )

import Crypto.Hash exposing (sha256)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import ZapMeme.Types
    exposing
        ( Caption
        , Image
        , Inputs
        , Meme
        , SavedModel
        , TextAlignment(..)
        , TextPosition(..)
        )


encodeMeme : Meme -> Value
encodeMeme meme =
    JE.object
        [ ( "image", encodeImage meme.image )
        , ( "captions", JE.list encodeCaption meme.captions )
        , ( "height", JE.int meme.height )
        , ( "width", JE.int meme.width )
        ]


decodeMeme : Value -> Result JD.Error Meme
decodeMeme value =
    JD.decodeValue memeDecoder value


memeDecoder : Decoder Meme
memeDecoder =
    JD.map4 Meme
        --hash needs to be looked up
        (JD.field "image" imageDecoder)
        (JD.field "captions" <| JD.list captionDecoder)
        (JD.field "height" JD.int)
        (JD.field "width" JD.int)


encodeImage : Image -> Value
encodeImage image =
    JE.object [ ( "url", JE.string image.url ) ]


imageDecoder : Decoder Image
imageDecoder =
    JD.map Image
        (JD.field "url" JD.string)


encodeCaption : Caption -> Value
encodeCaption caption =
    JE.object
        [ ( "text", JE.string caption.text )
        , ( "position", encodeTextPosition caption.position )
        , ( "alignment", encodeTextAlignment caption.alignment )
        , ( "font", JE.string caption.font )
        , ( "fontsize", JE.float caption.fontsize )
        , ( "fontcolor", JE.string caption.fontcolor )
        , ( "outlineColor"
          , case caption.outlineColor of
                Just color ->
                    JE.string color

                Nothing ->
                    JE.null
          )
        , ( "bold", JE.bool caption.bold )
        , ( "width", JE.int caption.width )
        , ( "height", JE.int caption.height )
        ]


captionDecoder : Decoder Caption
captionDecoder =
    JD.succeed Caption
        |> required "text" JD.string
        |> required "position" textPositionDecoder
        |> required "alignment" textAlignmentDecoder
        |> required "font" JD.string
        |> required "fontsize" JD.float
        |> required "fontcolor" JD.string
        |> required "outlineColor" (JD.nullable JD.string)
        |> required "bold" JD.bool
        |> required "width" JD.int
        |> required "height" JD.int


encodeTextPosition : TextPosition -> Value
encodeTextPosition position =
    case position of
        ExplicitPosition x y ->
            JE.object
                [ ( "x", JE.int x )
                , ( "y", JE.int y )
                ]

        _ ->
            JE.string <|
                case position of
                    TopLeft ->
                        "TopLeft"

                    TopCenter ->
                        "TopCenter"

                    TopRight ->
                        "TopRight"

                    MiddleLeft ->
                        "MiddleLeft"

                    MiddleCenter ->
                        "MiddleCenter"

                    MiddleRight ->
                        "MiddleRight"

                    BottomLeft ->
                        "BottomLeft"

                    BottomCenter ->
                        "BottomCenter"

                    BottomRight ->
                        "BottomRight"

                    _ ->
                        "impossible position"


textPositionDecoder : Decoder TextPosition
textPositionDecoder =
    JD.oneOf
        [ JD.map2 ExplicitPosition
            (JD.field "x" JD.int)
            (JD.field "y" JD.int)
        , JD.string |> JD.andThen textPositionHelp
        ]


textPositionHelp : String -> Decoder TextPosition
textPositionHelp s =
    case s of
        "TopLeft" ->
            JD.succeed TopLeft

        "TopCenter" ->
            JD.succeed TopCenter

        "TopRight" ->
            JD.succeed TopRight

        "MiddleLeft" ->
            JD.succeed MiddleLeft

        "MiddleCenter" ->
            JD.succeed MiddleCenter

        "MiddleRight" ->
            JD.succeed MiddleRight

        "BottomLeft" ->
            JD.succeed BottomLeft

        "BottomCenter" ->
            JD.succeed BottomCenter

        "BottomRight" ->
            JD.succeed BottomRight

        _ ->
            JD.fail <| "Unknown position: " ++ s


encodeTextAlignment : TextAlignment -> Value
encodeTextAlignment alignment =
    JE.string <|
        case alignment of
            Left ->
                "Left"

            Right ->
                "Right"

            Center ->
                "Center"


textAlignmentDecoder : Decoder TextAlignment
textAlignmentDecoder =
    JD.string |> JD.andThen textAlignmentHelp


textAlignmentHelp : String -> Decoder TextAlignment
textAlignmentHelp s =
    case s of
        "Left" ->
            JD.succeed Left

        "Right" ->
            JD.succeed Right

        "Center" ->
            JD.succeed Center

        _ ->
            JD.fail <| "Unknown TextAlignment: " ++ s


encodeSavedModel : SavedModel -> Value
encodeSavedModel model =
    JE.object
        [ ( "selectedPosition"
          , case model.selectedPosition of
                Nothing ->
                    JE.null

                Just position ->
                    encodeTextPosition position
          )
        , ( "showCaptionBorders", JE.bool model.showCaptionBorders )
        , ( "maxWidth", JE.int model.maxWidth )
        , ( "maxHeight", JE.int model.maxHeight )
        , ( "inputs", encodeInputs model.inputs )
        , ( "showHelp", JE.bool model.showHelp )
        ]


decodeSavedModel : Value -> Result JD.Error SavedModel
decodeSavedModel value =
    JD.decodeValue savedModelDecoder value


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> required "selectedPosition" (JD.nullable textPositionDecoder)
        |> required "showCaptionBorders" JD.bool
        |> required "maxWidth" JD.int
        |> required "maxHeight" JD.int
        |> required "inputs" inputsDecoder
        |> required "showHelp" JD.bool


encodeInputs : Inputs -> Value
encodeInputs inputs =
    let
        imageUrl =
            if String.startsWith "data:" inputs.imageUrl then
                ""

            else
                inputs.imageUrl
    in
    JE.object
        [ ( "text", JE.string inputs.text )
        , ( "imageUrl", JE.string imageUrl )
        , ( "position", encodeTextPosition inputs.position )
        , ( "alignment", encodeTextAlignment inputs.alignment )
        , ( "font", JE.string inputs.font )
        , ( "fontsize", JE.string inputs.fontsize )
        , ( "fontcolor", JE.string inputs.fontcolor )
        , ( "isOutlined", JE.bool inputs.isOutlined )
        , ( "outlineColor", JE.string inputs.outlineColor )
        , ( "bold", JE.bool inputs.bold )
        , ( "width", JE.string inputs.width )
        , ( "height", JE.string inputs.height )
        , ( "fileName", JE.string inputs.fileName )
        ]


inputsDecoder : Decoder Inputs
inputsDecoder =
    JD.succeed Inputs
        |> required "text" JD.string
        |> required "imageUrl" JD.string
        |> required "position" textPositionDecoder
        |> required "alignment" textAlignmentDecoder
        |> required "font" JD.string
        |> required "fontsize" JD.string
        |> required "fontcolor" JD.string
        |> required "isOutlined" JD.bool
        |> required "outlineColor" JD.string
        |> required "bold" JD.bool
        |> required "width" JD.string
        |> required "height" JD.string
        |> required "fileName" JD.string
