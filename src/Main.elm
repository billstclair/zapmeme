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
import Svg exposing (Svg, g, line, rect, svg)
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
    | TopMiddle
    | TopRight
    | CenterLeft
    | CenterMiddle
    | CenterRight
    | BottomLeft
    | BottomMiddle
    | BottomRight
    | ExplicitPosition Int Int


type alias Caption =
    { text : String
    , position : TextPosition
    , size : Int
    , width : String
    }


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


emptyMeme : Meme
emptyMeme =
    { image = initialImage
    , captions = []
    , width = 1118
    , height = 671
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
            ]
        ]
    }


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
    svg [ height h, width w ]
        [ Svg.image [ xlinkHref url ]
            []
        ]


codestr code =
    String.fromList [ Char.fromCode code ]


chars =
    { leftCurlyQuote = codestr 0x201C
    , copyright = codestr 0xA9
    , nbsp = codestr 0xA0
    }
