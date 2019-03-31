---------------------------------------------------------------------
--
-- Types.elm
-- Shared types.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ZapMeme.Types exposing
    ( Caption
    , Font
    , Image
    , Meme
    , TextAlignment(..)
    , TextPosition(..)
    )

{-| fontsize, width, and height are percentages of the image size
-}


type TextAlignment
    = Left
    | Right
    | Center


type TextPosition
    = TopLeft
    | TopCenter
    | TopRight
    | MiddleLeft
    | MiddleCenter
    | MiddleRight
    | BottomLeft
    | BottomCenter
    | BottomRight
    | ExplicitPosition Int Int


type alias Caption =
    { text : String
    , position : TextPosition
    , alignment : TextAlignment
    , font : String
    , fontsize : Float
    , fontcolor : String
    , bold : Bool
    , width : Int
    , height : Int
    }


{-| Packaged as a type, since it may change.
-}
type alias Image =
    { url : String
    }


type alias Meme =
    { image : Image
    , captions : List Caption
    , height : Int
    , width : Int
    }


type alias Font =
    { font : String
    , family : String
    }
