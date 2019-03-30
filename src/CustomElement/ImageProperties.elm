----------------------------------------------------------------------
--
-- ImageProperties.elm
-- Elm interface to <image-properties> custom element.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module CustomElement.ImageProperties exposing
    ( ImageProperties
    , imageProperties
    , imageId, triggerImageProperties
    , onImageProperties
    , propertiesDecoder
    )

{-| The Elm interface to the `image-properties` custom element.

This code won't do anything unless `site/js/image-properties.js` is loaded.


# Types

@docs ImageProperties


# Html Elements

@docs imageProperties


# Attributes

@docs imageId, triggerImageProperties


# Events

@docs onImageProperties


# Decoders

@docs propertiesDecoder

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (property)
import Html.Events exposing (on)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| The value for the `onImageProperties` event.
-}
type alias ImageProperties =
    { id : String
    , width : Int
    , height : Int
    }


{-| Create an `image-properties` HTML element
-}
imageProperties : List (Attribute msg) -> List (Html msg) -> Html msg
imageProperties =
    Html.node "image-properties"


{-| This is how you set the id of the tracked image element.
-}
imageId : String -> Attribute msg
imageId value =
    property "imageId" <|
        JE.string value


{-| This is how you trigger the event for the properties.
-}
triggerImageProperties : Int -> Attribute msg
triggerImageProperties value =
    property "triggerImageProperties" <|
        JE.int value


{-| This is how you receive the caret selection and coordinates.
-}
onImageProperties : (ImageProperties -> msg) -> Attribute msg
onImageProperties tagger =
    on "imageProperties" <|
        JD.map tagger <|
            JD.at [ "target", "imageProperties" ] propertiesDecoder



---
--- JSON encoders and decoders
---


{-| Decoder for the `ImageProperties` type.
-}
propertiesDecoder : Decoder ImageProperties
propertiesDecoder =
    JD.value
        |> JD.andThen propertiesDecoderDebug


propertiesDecoderDebug : Value -> Decoder ImageProperties
propertiesDecoderDebug value =
    JD.map3 ImageProperties
        (JD.field "id" JD.string)
        (JD.field "width" JD.int)
        (JD.field "height" JD.int)
