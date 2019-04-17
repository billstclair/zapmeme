----------------------------------------------------------------------
--
-- IsFontAvailable.elm
-- Elm interface to <is-font-available> custom element.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module CustomElement.IsFontAvailable exposing
    ( AvailableFonts
    , isFontAvailable
    , fonts, trigger
    , onAvailableFonts
    , availableFontsDecoder
    )

{-| The Elm interface to the `image-properties` custom element.

This code won't do anything unless `site/js/image-properties.js` is loaded.


# Types

@docs AvailableFonts


# Html Elements

@docs isFontAvailable


# Attributes

@docs fonts, trigger


# Events

@docs onAvailableFonts


# Decoders

@docs availableFontsDecoder

-}

import Html exposing (Attribute, Html)
import Html.Attributes exposing (property)
import Html.Events exposing (on)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| The value for the `onAvailableFonts` event.

    [(<font-name>, <isAvailable>), ...]

-}
type alias AvailableFonts =
    List ( String, Bool )


{-| Create an 'isFontAvailable\` HTML element
-}
isFontAvailable : List (Attribute msg) -> List (Html msg) -> Html msg
isFontAvailable =
    Html.node "is-font-available"


{-| This is how you set the list of fonts to query.
-}
fonts : List String -> Attribute msg
fonts list =
    property "fonts" <|
        JE.list JE.string list


{-| This is how you trigger the event for the properties.
-}
trigger : Int -> Attribute msg
trigger value =
    property "trigger" <|
        JE.int value


{-| This is how you receive the image properties.
-}
onAvailableFonts : (AvailableFonts -> msg) -> Attribute msg
onAvailableFonts tagger =
    on "availableFonts" <|
        JD.map tagger <|
            JD.at [ "target", "availableFonts" ] availableFontsDecoder



---
--- JSON encoders and decoders
---


{-| Decoder for the `availableFonts` type.
-}
availableFontsDecoder : Decoder AvailableFonts
availableFontsDecoder =
    JD.keyValuePairs JD.bool
