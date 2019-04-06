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
    , Inputs
    , Meme
    , SavedModel
    , TextAlignment(..)
    , TextPosition(..)
    , WhichDialog(..)
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
    , outlineColor : Maybe String
    , bold : Bool
    , width : Int
    , height : Int
    }


type alias Inputs =
    { -- For the selected caption
      text : String
    , imageUrl : String
    , position : TextPosition
    , alignment : TextAlignment
    , font : String
    , fontsize : String
    , fontcolor : String
    , isOutlined : Bool
    , outlineColor : String
    , bold : Bool
    , width : String
    , height : String
    , fileName : String
    }


{-| Packaged as a type, since it may change.
-}
type alias Image =
    { url : String
    , hash : String
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


type WhichDialog
    = NoDialog
    | HelpDialog
    | MemesDialog
    | ImagesDialog
    | FontsDialog
    | DataDialog


{-| This does not include the current meme.

That's saved as the "meme" key.

-}
type alias SavedModel =
    { dialog : WhichDialog
    , selectedPosition : Maybe TextPosition
    , savedSelectedPosition : Maybe TextPosition
    , showCaptionBorders : Bool
    , maxWidth : Int
    , maxHeight : Int
    , inputs : Inputs
    , showMemeImage : Bool
    , showHelp : Bool
    }
