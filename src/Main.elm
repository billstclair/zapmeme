---------------------------------------------------------------------
--
-- Main.elm
-- Meme Machine top-level
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , blockquote
        , button
        , div
        , fieldset
        , h2
        , h3
        , h4
        , img
        , input
        , label
        , p
        , span
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( align
        , alt
        , checked
        , colspan
        , disabled
        , height
        , href
        , name
        , placeholder
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import PortFunnel.LocalStorage as LocalStorage
import PortFunnels exposing (FunnelDict, Handler(..))
import Svg exposing (Svg, foreignObject, g, line, rect, svg)
import Svg.Attributes
    exposing
        ( fill
        , fontSize
        , height
        , stroke
        , transform
        , width
        , x
        , x1
        , x2
        , xlinkHref
        , y
        , y1
        , y2
        )
import Url exposing (Url)


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


type TextAlignment
    = Left
    | Right
    | Center


textAlignmentString : TextAlignment -> String
textAlignmentString alignment =
    case alignment of
        Left ->
            "left"

        Right ->
            "right"

        Center ->
            "center"


type alias Caption =
    { text : String
    , position : TextPosition
    , alignment : TextAlignment
    , font : String
    , fontsize : Int
    , fontcolor : String
    , bold : Bool
    , width : Int
    , height : Int
    }


captionXY : Caption -> Int -> Int -> ( Int, Int )
captionXY caption totalWidth totalHeight =
    let
        w =
            caption.width

        wo2 =
            w // 2

        h =
            caption.height

        ho2 =
            h // 2

        totalCenter =
            totalWidth // 2

        totalMiddle =
            totalHeight // 2

        centerx =
            totalCenter - wo2

        rightx =
            totalWidth - w

        middley =
            totalMiddle - ho2

        bottomy =
            totalHeight - h
    in
    case caption.position of
        TopLeft ->
            ( 0, 0 )

        TopCenter ->
            ( centerx, 0 )

        TopRight ->
            ( rightx, 0 )

        MiddleLeft ->
            ( 0, middley )

        MiddleCenter ->
            ( centerx, middley )

        MiddleRight ->
            ( rightx, middley )

        BottomLeft ->
            ( 0, bottomy )

        BottomCenter ->
            ( centerx, bottomy )

        BottomRight ->
            ( rightx, bottomy )

        ExplicitPosition x y ->
            ( x, y )


{-| Packaged as a type, since it may change.
-}
type alias Image =
    { url : String
    }


initialImage =
    { url = "images/is-this-a-pigeon.jpg"
    }


type alias Meme =
    { image : Image
    , captions : List Caption
    , height : Int
    , width : Int
    }


sampleCaptions : List Caption
sampleCaptions =
    [ { text = "Is this a pigeon?"
      , position = BottomCenter
      , alignment = Center
      , font = "avant-garde"
      , fontsize = 50
      , fontcolor = "white"
      , bold = True
      , width = 500
      , height = 100
      }
    ]


emptyMeme : Meme
emptyMeme =
    { image = initialImage
    , captions = sampleCaptions
    , width = 839
    , height = 503
    }


type alias Model =
    { meme : Meme
    , key : Key
    , funnelState : PortFunnels.State
    , msg : Maybe String
    }


type Msg
    = Noop
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | ProcessLocalStorage Value


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = HandleUrlRequest
        , onUrlChange = HandleUrlChange
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PortFunnels.subscriptions ProcessLocalStorage model
        ]


localStoragePrefix : String
localStoragePrefix =
    "elm-meme-maker"


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    { meme = emptyMeme
    , key = key
    , funnelState = PortFunnels.initialState localStoragePrefix
    , msg = Nothing
    }
        |> withNoCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        HandleUrlRequest request ->
            ( model
            , case request of
                Internal url ->
                    -- For now
                    Navigation.load <| Url.toString url

                External urlString ->
                    Navigation.load urlString
            )

        HandleUrlChange url ->
            model |> withNoCmd

        ProcessLocalStorage value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    { model | msg = Just error } |> withNoCmd

                Ok res ->
                    res


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict [ LocalStorageHandler storageHandler ] getCmdPort


getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort moduleName model =
    PortFunnels.getCmdPort ProcessLocalStorage moduleName False


localStorageSend : LocalStorage.Message -> Model -> Cmd Msg
localStorageSend message model =
    LocalStorage.send (getCmdPort LocalStorage.moduleName model)
        message
        model.funnelState.storage


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    model |> withNoCmd


br : Html msg
br =
    Html.br [] []


defaultFont : Font
defaultFont =
    Font "arial-black" "\"Arial Black\",helvetica,sans-serif"


{-| Started life as <http://web.mit.edu/jmorzins/www/@/css/fonts.css>
-}
safeFontPairs : List ( String, String )
safeFontPairs =
    [ ( "helvetica", "helvetica,sans-serif" )
    , ( "arial", "arial,helvetica,sans-serif" )
    , ( "verdana", "verdana,arial,helvetica,sans-serif" )
    , ( "tahoma", "tahoma,arial,helvetica,sans-serif" )
    , ( "arial-black", "\"Arial Black\",helvetica,sans-serif" )
    , ( "comic-sans-ms", "\"Comic Sans MS\",arial,helvetica,sans-serif" )
    , ( "trebuchet-ms", "\"Trebuchet MS\",arial,helvetica,sans-serif" )
    , ( "impact", "impact,helvetica,sans-serif" )
    , ( "courier", "courier,monospace" )
    , ( "courier-new", "\"courier new\",courier,monospace" )
    , ( "andale-mono", "\"andale mono\",\"monotype.com\",monaco,\"courier new\",courier,monospace" )
    , ( "georgia", "georgia,times,serif" )
    , ( "times", "\"Times Roman\",times,serif" )
    , ( "times-new-roman", "\"Times New Roman\",\"Times Roman\",TimesNR,times,serif" )
    , ( "palatino", "\"Palatino Linotype\",\"URW Palladio L\",\"palladio l\",palatino,\"book antiqua\",times,serif" )
    , ( "century-schoolbook", "\"Century Schoolbook\",Century,\"new century schoolbook\",\"Century Schoolbook L\",times,serif" )
    , ( "bookman", "\"Bookman Old Style\",\"URW Bookman L\",\"itc bookman\",times,serif" )
    , ( "garamond", "Garamond,\"Garamond Antiqua\",times,serif" )
    , ( "avant-garde", "\"Century Gothic\",\"Avant Garde Gothic\",\"Avant Garde\",\"URW Gothic L\",helvetica,sans-serif" )
    ]


type alias Font =
    { font : String
    , family : String
    }


safeFontList : List Font
safeFontList =
    safeFontPairs
        |> List.sort
        |> List.map (\( font, family ) -> Font font family)


safeFontDict : Dict String Font
safeFontDict =
    safeFontList
        |> List.map (\f -> ( f.font, f ))
        |> Dict.fromList


safeFonts : List String
safeFonts =
    List.map .font safeFontList


fontAttribute : Font -> Attribute msg
fontAttribute font =
    style "font-family" font.family


view : Model -> Document Msg
view model =
    { title = "Elm Meme Maker"
    , body =
        [ div [ align "center" ]
            [ h2 [] [ text "Elm Meme Maker" ]
            , p []
                [ renderMeme model.meme ]
            , p []
                [ text <| chars.copyright ++ " 2019 Bill St. Clair"
                , br
                , a [ href "https://github.com/billstclair/elm-meme-maker" ]
                    [ text "GitHub" ]
                ]
            , safeFontParagraph
            ]
        ]
    }


safeFontParagraph : Html msg
safeFontParagraph =
    p [] <|
        List.concat
            [ [ span
                    [ style "font-size" "110%"
                    , style "font-weight" "bold"
                    ]
                    [ text "Safe Fonts" ]
              , br
              ]
            , List.map fontExample safeFontList
            ]


fontExample : Font -> Html msg
fontExample font =
    span []
        [ span [ fontAttribute font ]
            [ text font.font ]
        , text " "
        ]


renderMeme : Meme -> Html Msg
renderMeme meme =
    let
        image =
            meme.image

        h =
            String.fromInt meme.height

        w =
            String.fromInt meme.width

        url =
            meme.image.url
    in
    svg [ width w, height h ] <|
        List.concat
            [ [ Svg.image
                    [ width w, height h, xlinkHref url ]
                    []
              ]
            , List.map (renderCaption meme) meme.captions
            ]


tos : Int -> String
tos int =
    String.fromInt int


renderCaption : Meme -> Caption -> Svg msg
renderCaption meme caption =
    let
        ( cx, cy ) =
            captionXY caption meme.width meme.height

        alignment =
            textAlignmentString caption.alignment

        font =
            Maybe.withDefault defaultFont <| Dict.get caption.font safeFontDict

        weight =
            if caption.bold then
                "bold"

            else
                "normal"
    in
    foreignObject
        [ x (tos cx)
        , y (tos cy)
        , width (tos caption.width)
        , height (tos caption.height)
        ]
        [ div
            [ style "text-align" alignment
            , fontAttribute font
            , style "font-size" <| tos caption.fontsize ++ "px"
            , style "color" caption.fontcolor
            , style "font-weight" weight
            ]
            [ text caption.text ]
        ]


codestr code =
    String.fromList [ Char.fromCode code ]


chars =
    { leftCurlyQuote = codestr 0x201C
    , copyright = codestr 0xA9
    , nbsp = codestr 0xA0
    }
