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
    , decodeStorageMirror
    , encodeMeme
    , encodeSavedModel
    , encodeStorageMirror
    , encodeWhichDialog
    , memeDecoder
    , savedModelDecoder
    , storageMirrorDecoder
    )

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
        , StorageMirror
        , TextAlignment(..)
        , TextPosition(..)
        , WhichDialog(..)
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
    JE.object [ ( "hash", JE.string image.hash ) ]


imageDecoder : Decoder Image
imageDecoder =
    JD.map
        (\hash ->
            Image "" hash
        )
        (JD.field "hash" JD.string)


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


encodeWhichDialog : WhichDialog -> Value
encodeWhichDialog dialog =
    case dialog of
        NoDialog ->
            JE.string "NoDialog"

        MemesDialog ->
            JE.string "MemesDialog"

        HelpDialog ->
            JE.string "HelpDialog"

        ImagesDialog ->
            JE.string "ImagesDialog"

        FontsDialog ->
            JE.string "FontsDialog"

        DataDialog ->
            JE.string "DataDialog"


whichDialogDecoder : Decoder WhichDialog
whichDialogDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "HelpDialog" ->
                        JD.succeed HelpDialog

                    "MemesDialog" ->
                        JD.succeed MemesDialog

                    "ImagesDialog" ->
                        JD.succeed ImagesDialog

                    "FontsDialog" ->
                        JD.succeed FontsDialog

                    "DataDialog" ->
                        JD.succeed DataDialog

                    _ ->
                        JD.succeed NoDialog
            )


encodeSavedModel : SavedModel -> Value
encodeSavedModel model =
    JE.object
        [ ( "dialog", encodeWhichDialog model.dialog )
        , ( "selectedPosition"
          , case model.selectedPosition of
                Nothing ->
                    JE.null

                Just position ->
                    encodeTextPosition position
          )
        , ( "savedSelectedPosition"
          , case model.savedSelectedPosition of
                Nothing ->
                    JE.null

                Just position ->
                    encodeTextPosition position
          )
        , ( "showCaptionBorders", JE.bool model.showCaptionBorders )
        , ( "maxWidth", JE.int model.maxWidth )
        , ( "maxHeight", JE.int model.maxHeight )
        , ( "inputs", encodeInputs model.inputs )
        , ( "showMemeImage", JE.bool model.showMemeImage )
        , ( "showHelp", JE.bool model.showHelp )
        ]


decodeSavedModel : Value -> Result JD.Error SavedModel
decodeSavedModel value =
    JD.decodeValue savedModelDecoder value


{-| Optional fields below were added after the code shipped without them.
-}
savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> optional "dialog" whichDialogDecoder NoDialog
        |> required "selectedPosition" (JD.nullable textPositionDecoder)
        |> optional "savedSelectedPosition" (JD.nullable textPositionDecoder) Nothing
        |> required "showCaptionBorders" JD.bool
        |> required "maxWidth" JD.int
        |> required "maxHeight" JD.int
        |> required "inputs" inputsDecoder
        |> optional "showMemeImage" JD.bool False
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
        , ( "savedMemeName", JE.string inputs.savedMemeName )
        , ( "showAllImages", JE.bool inputs.showAllImages )
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
        |> optional "savedMemeName" JD.string ""
        |> optional "showAllImages" JD.bool True



{- type alias StorageMirror =
   { images : List ( String, String )
   , memes : List ( String, Meme )
   }
-}


encodeStorageMirror : StorageMirror -> Value
encodeStorageMirror mirror =
    JE.object
        [ ( "memes"
          , List.map (\( name, meme ) -> ( name, encodeMeme meme )) mirror.memes
                |> JE.object
          )
        , ( "images"
          , List.map (\( hash, url ) -> ( hash, JE.string url )) mirror.images
                |> JE.object
          )
        ]


storageMirrorDecoder : Decoder StorageMirror
storageMirrorDecoder =
    JD.map2 StorageMirror
        (JD.field "memes" <| JD.keyValuePairs memeDecoder)
        (JD.field "images" <| JD.keyValuePairs JD.string)


decodeStorageMirror : Value -> Result JD.Error StorageMirror
decodeStorageMirror value =
    JD.decodeValue storageMirrorDecoder value
