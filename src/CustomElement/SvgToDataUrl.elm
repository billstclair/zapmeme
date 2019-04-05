----------------------------------------------------------------------
--
-- SvgToDataUrl.elm
-- Elm interface to <svg-to-data-url> custom element.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module CustomElement.SvgToDataUrl exposing
    ( ReturnedUrl, ReturnedFile
    , svgToDataUrl
    , returnedUrlParameters, triggerReturnedUrl
    , returnedFileParameters, triggerReturnedFile
    , onReturnedUrl, onReturnedFile
    , returnedUrlDecoder, returnedFileDecoder
    )

{-| The Elm interface to the `svg-to-data-url` custom element.

This code won't do anything unless `site/js/svg-to-data-url.js` is loaded.


# Types

@docs ReturnedUrl, ReturnedFile


# Html Elements

@docs svgToDataUrl


# Attributes

@docs returnedUrlParameters, triggerReturnedUrl
@docs returnedFileParameters, triggerReturnedFile


# Events

@docs onReturnedUrl, onReturnedFile


# Decoders

@docs returnedUrlDecoder, returnedFileDecoder

-}

import File exposing (File)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (property)
import Html.Events exposing (on)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| The value for the `onReturnedUrl` event.
-}
type alias ReturnedUrl =
    { svgId : String
    , mimeType : String
    , url : String
    }


{-| The value for the `onReturnedFile` event.
-}
type alias ReturnedFile =
    { svgId : String
    , fileName : String
    , mimeType : String
    , canDownload : Bool
    , file : File
    }


{-| Create an `svg-to-data-url` HTML element
-}
svgToDataUrl : List (Attribute msg) -> List (Html msg) -> Html msg
svgToDataUrl =
    Html.node "svg-to-data-url"


{-| Set parameters for triggerReturnedUrl.
-}
returnedUrlParameters : String -> String -> Attribute msg
returnedUrlParameters svgId mimeType =
    property "returnedUrlParameters" <|
        JE.object
            [ ( "svgId", JE.string svgId )
            , ( "mimeType", JE.string mimeType )
            ]


{-| This is how you trigger the onReturnedUrl event.
-}
triggerReturnedUrl : Int -> Attribute msg
triggerReturnedUrl trigger =
    property "triggerReturnedUrl" <|
        JE.int trigger


{-| Set parameters for triggerReturnedFile.
-}
returnedFileParameters : String -> String -> String -> Attribute msg
returnedFileParameters svgId fileName mimeType =
    property "returnedFileParameters" <|
        JE.object
            [ ( "svgId", JE.string svgId )
            , ( "fileName", JE.string fileName )
            , ( "mimeType", JE.string mimeType )
            ]


{-| This is how you trigger the onReturnedFile event.
-}
triggerReturnedFile : Int -> Attribute msg
triggerReturnedFile trigger =
    property "triggerReturnedFile" <|
        JE.int trigger


{-| This is how you receive the result of `triggerReturnedUrl`.
-}
onReturnedUrl : (ReturnedUrl -> msg) -> Attribute msg
onReturnedUrl tagger =
    on "returnedUrl" <|
        JD.map tagger <|
            JD.at [ "target", "returnedUrl" ] returnedUrlDecoder


{-| This is how you receive the result of `triggerReturnedFile`.
-}
onReturnedFile : (ReturnedFile -> msg) -> Attribute msg
onReturnedFile tagger =
    on "returnedFile" <|
        JD.map tagger <|
            JD.at [ "target", "returnedFile" ] returnedFileDecoder



---
--- JSON encoders and decoders
---


{-| Decoder for the `ReturnedUrl` type.
-}
returnedUrlDecoder : Decoder ReturnedUrl
returnedUrlDecoder =
    JD.value
        |> JD.andThen returnedUrlDecoderDebug


returnedUrlDecoderDebug : Value -> Decoder ReturnedUrl
returnedUrlDecoderDebug value =
    JD.map3 ReturnedUrl
        (JD.field "svgId" JD.string)
        (JD.field "mimeType" JD.string)
        (JD.field "url" JD.string)


{-| Decoder for the `ReturnedFile` type.
-}
returnedFileDecoder : Decoder ReturnedFile
returnedFileDecoder =
    JD.value
        |> JD.andThen returnedFileDecoderDebug


returnedFileDecoderDebug : Value -> Decoder ReturnedFile
returnedFileDecoderDebug value =
    JD.map5 ReturnedFile
        (JD.field "svgId" JD.string)
        (JD.field "fileName" JD.string)
        (JD.field "mimeType" JD.string)
        (JD.field "canDownload" JD.bool)
        (JD.field "file" File.decoder)
