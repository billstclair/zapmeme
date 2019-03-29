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
        , optgroup
        , option
        , p
        , select
        , span
        , table
        , td
        , text
        , textarea
        , tr
        )
import Html.Attributes
    exposing
        ( align
        , alt
        , checked
        , cols
        , colspan
        , disabled
        , height
        , href
        , id
        , name
        , placeholder
        , rows
        , selected
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (on, onCheck, onClick, onInput)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import List.Extra as LE
import PortFunnel.LocalStorage as LocalStorage
import PortFunnels exposing (FunnelDict, Handler(..))
import Svg exposing (Svg, foreignObject, g, line, rect, svg)
import Svg.Attributes
    exposing
        ( fill
        , fontSize
        , height
        , stroke
        , strokeDasharray
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
import Svg.Events
import Task
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


textPositionToString : TextPosition -> String
textPositionToString position =
    case position of
        TopLeft ->
            "Top Left"

        TopCenter ->
            "Top Center"

        TopRight ->
            "Top Right"

        MiddleLeft ->
            "Middle Left"

        MiddleCenter ->
            "Middle Center"

        MiddleRight ->
            "Middle Right"

        BottomLeft ->
            "Bottom Left"

        BottomCenter ->
            "Bottom Center"

        BottomRight ->
            "Bottom Right"

        ExplicitPosition x y ->
            ""


stringToTextPosition : String -> TextPosition
stringToTextPosition string =
    case string of
        "Top Left" ->
            TopLeft

        "Top Center" ->
            TopCenter

        "Top Right" ->
            TopRight

        "Middle Left" ->
            MiddleLeft

        "Middle Center" ->
            MiddleCenter

        "Middle Right" ->
            MiddleRight

        "Bottom Left" ->
            BottomLeft

        "Bottom Center" ->
            BottomCenter

        "Bottom Right" ->
            BottomRight

        _ ->
            TopCenter


type TextAlignment
    = Left
    | Right
    | Center


allAlignments : List TextAlignment
allAlignments =
    [ Left, Center, Right ]


alignmentToString : TextAlignment -> String
alignmentToString alignment =
    case alignment of
        Left ->
            "Left"

        Right ->
            "Right"

        Center ->
            "Center"


stringToAlignment : String -> TextAlignment
stringToAlignment string =
    case string of
        "Left" ->
            Left

        "Right" ->
            Right

        "Center" ->
            Center

        _ ->
            Center


{-| fontsize, widdth, and height are percentages of the image size
-}
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


captionCoordinates : Caption -> Int -> Int -> ( ( Int, Int ), ( Int, Int ) )
captionCoordinates caption totalWidth totalHeight =
    let
        w =
            caption.width * totalWidth // 100

        wo2 =
            w // 2

        h =
            caption.height * totalHeight // 100

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

        topy =
            2

        middley =
            totalMiddle - ho2

        bottomy =
            totalHeight - h - 1

        position =
            case caption.position of
                TopLeft ->
                    ( 0, topy )

                TopCenter ->
                    ( centerx, topy )

                TopRight ->
                    ( rightx, topy )

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
    in
    ( position, ( w, h ) )


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
    [ { text = "I ask you<br>once again:"
      , position = TopCenter
      , alignment = Center
      , font = "avant-garde"
      , fontsize = 10
      , fontcolor = "white"
      , bold = True
      , width = 75
      , height = 30
      }
    , { text = "Is this a pigeon?"
      , position = BottomCenter
      , alignment = Center
      , font = "avant-garde"
      , fontsize = 10
      , fontcolor = "white"
      , bold = True
      , width = 75
      , height = 15
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
    , selectedPosition : Maybe TextPosition
    , deletedCaption : Maybe Caption
    , showCaptionBorders : Bool
    , inputs : Inputs
    , fontDict : Dict String Font
    , key : Key
    , funnelState : PortFunnels.State
    , msg : Maybe String
    }


type alias Inputs =
    { -- For the selected caption
      text : String
    , position : TextPosition
    , alignment : TextAlignment
    , font : String
    , fontsize : String
    , fontcolor : String
    , bold : Bool
    , width : String
    , height : String
    }


initialInputs : Inputs
initialInputs =
    { text = ""
    , position = TopCenter
    , alignment = Center
    , font = "avante-garde"
    , fontsize = "10"
    , fontcolor = "white"
    , bold = True
    , width = "75"
    , height = "15"
    }


type Msg
    = Noop
    | SelectCaption (Maybe TextPosition)
    | AddCaption
    | DeleteCaption
    | UndoDeletion
    | SetShowCaptionBorders Bool
    | SetText String
    | SetPosition TextPosition
    | SetPositionString String
    | SetAlignment TextAlignment
    | SetAlignmentString String
    | SetFont String
    | SetFontSize String
    | SetFontColor String
    | SetBold Bool
    | SetWidth String
    | SetHeight String
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
    , deletedCaption = Nothing
    , selectedPosition = Nothing
    , showCaptionBorders = True
    , inputs = initialInputs
    , fontDict = safeFontDict
    , key = key
    , funnelState = PortFunnels.initialState localStoragePrefix
    , msg = Nothing
    }
        |> withNoCmd


findCaption : Maybe TextPosition -> List Caption -> Maybe Caption
findCaption currentPosition captions =
    LE.find (\c -> currentPosition == Just c.position) captions


selectCaption : Maybe TextPosition -> Model -> ( Model, Cmd Msg )
selectCaption position model =
    let
        pos =
            if position == model.selectedPosition then
                Nothing

            else
                position

        inputs =
            if pos == Nothing then
                model.inputs

            else
                case findCaption pos model.meme.captions of
                    Nothing ->
                        model.inputs

                    Just caption ->
                        { text = caption.text
                        , position = caption.position
                        , alignment = caption.alignment
                        , font = caption.font
                        , fontsize = tos caption.fontsize
                        , fontcolor = caption.fontcolor
                        , bold = caption.bold
                        , width = tos caption.width
                        , height = tos caption.height
                        }
    in
    { model
        | selectedPosition = pos
        , inputs = inputs
        , deletedCaption = Nothing
    }
        |> withCmd (Task.attempt (\_ -> Noop) (Dom.focus "text"))


setPosition : TextPosition -> Model -> ( Model, Cmd Msg )
setPosition position model =
    let
        inputs =
            model.inputs

        selectedPosition =
            model.selectedPosition

        mdl =
            model |> updateCaption (\c -> { c | position = position })
    in
    { mdl
        | inputs =
            { inputs
                | position = position
            }
        , selectedPosition = Just position
    }
        |> withNoCmd


addCaption : Model -> ( Model, Cmd Msg )
addCaption model =
    let
        meme =
            model.meme

        captions =
            meme.captions

        positions =
            List.filter
                (\p -> Nothing == LE.find (\c -> p == c.position) captions)
                allPositions
    in
    case positions of
        [] ->
            model |> withNoCmd

        position :: _ ->
            let
                caption =
                    { text = "Nothing to see here"
                    , position = position
                    , alignment = Center
                    , font = "avant-garde"
                    , fontsize = 10
                    , fontcolor = "white"
                    , bold = True
                    , width = 75
                    , height = 15
                    }
            in
            { model
                | meme =
                    { meme
                        | captions = caption :: captions
                    }
                , deletedCaption = Nothing
            }
                |> selectCaption (Just position)


deleteCaption : Model -> ( Model, Cmd Msg )
deleteCaption model =
    let
        meme =
            model.meme

        selectedPosition =
            model.selectedPosition
    in
    { model
        | meme =
            { meme
                | captions =
                    List.filter (\c -> selectedPosition /= Just c.position)
                        meme.captions
            }
        , selectedPosition = Nothing
        , deletedCaption = findCaption selectedPosition meme.captions
    }
        |> withNoCmd


undoDeletion : Model -> ( Model, Cmd Msg )
undoDeletion model =
    case model.deletedCaption of
        Nothing ->
            model |> withNoCmd

        Just caption ->
            let
                meme =
                    model.meme
            in
            { model
                | meme =
                    { meme | captions = caption :: meme.captions }
                , selectedPosition = Just caption.position
                , deletedCaption = Nothing
            }
                |> withNoCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        inputs =
            model.inputs
    in
    case msg of
        Noop ->
            model |> withNoCmd

        AddCaption ->
            addCaption model

        DeleteCaption ->
            deleteCaption model

        UndoDeletion ->
            undoDeletion model

        SelectCaption position ->
            selectCaption position model

        SetShowCaptionBorders show ->
            { model | showCaptionBorders = show }
                |> withNoCmd

        SetText string ->
            { model
                | inputs =
                    { inputs | text = string }
            }
                |> updateCaption (\c -> { c | text = string })
                |> withNoCmd

        SetPosition position ->
            setPosition position model

        SetPositionString string ->
            let
                position =
                    stringToTextPosition string
            in
            setPosition position model

        SetAlignment alignment ->
            { model
                | inputs =
                    { inputs | alignment = alignment }
            }
                |> updateCaption (\c -> { c | alignment = alignment })
                |> withNoCmd

        SetAlignmentString string ->
            update (SetAlignment <| stringToAlignment string) model

        SetFont string ->
            { model
                | inputs =
                    { inputs | font = string }
            }
                |> updateCaption (\c -> { c | font = string })
                |> withNoCmd

        SetFontSize string ->
            { model
                | inputs =
                    { inputs | fontsize = string }
            }
                |> updateCaption (\c -> { c | fontsize = parseInt 10 string })
                |> withNoCmd

        SetFontColor string ->
            case string of
                "" ->
                    model |> withNoCmd

                _ ->
                    { model
                        | inputs =
                            { inputs | fontcolor = string }
                    }
                        |> updateCaption (\c -> { c | fontcolor = string })
                        |> withNoCmd

        SetBold bold ->
            { model
                | inputs =
                    { inputs | bold = bold }
            }
                |> updateCaption (\c -> { c | bold = bold })
                |> withNoCmd

        SetWidth string ->
            { model
                | inputs =
                    { inputs | width = string }
            }
                |> updateCaption (\c -> { c | width = parseInt 75 string })
                |> withNoCmd

        SetHeight string ->
            { model
                | inputs =
                    { inputs | height = string }
            }
                |> updateCaption (\c -> { c | height = parseInt 75 string })
                |> withNoCmd

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


parseInt : Int -> String -> Int
parseInt default string =
    Maybe.withDefault default <| String.toInt string


updateCaption : (Caption -> Caption) -> Model -> Model
updateCaption updater model =
    let
        meme =
            model.meme

        captions =
            meme.captions

        selectedPosition =
            model.selectedPosition
    in
    case findCaption selectedPosition captions of
        Nothing ->
            model

        Just caption ->
            { model
                | meme =
                    { meme
                        | captions =
                            updater caption
                                :: List.filter
                                    (\c -> selectedPosition /= Just c.position)
                                    captions
                    }
            }


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
                [ renderMeme model ]
            , p []
                [ renderInputs model ]
            , p []
                [ text <| chars.copyright ++ " 2019 Bill St. Clair"
                , br
                , a
                    [ href "https://github.com/billstclair/elm-meme-maker"
                    , target "_blank"
                    ]
                    [ text "GitHub" ]
                ]
            , safeFontParagraph
            ]
        ]
    }


th : String -> Html msg
th string =
    Html.th [ textalign "right" ]
        [ text string ]


textalign : String -> Attribute msg
textalign string =
    style "text-align" string


renderInputs : Model -> Html Msg
renderInputs model =
    let
        inputs =
            model.inputs

        isDisabled =
            model.selectedPosition == Nothing
    in
    table []
        [ tr []
            [ th "Caption Borders:"
            , td []
                [ input
                    [ type_ "checkbox"
                    , onCheck SetShowCaptionBorders
                    , checked model.showCaptionBorders
                    ]
                    []
                ]
            ]
        , tr []
            [ td [] []
            , td []
                [ button [ onClick AddCaption ]
                    [ text "Add Caption" ]
                , case model.selectedPosition of
                    Nothing ->
                        text ""

                    _ ->
                        button [ onClick DeleteCaption ]
                            [ text "Delete Caption" ]
                , case model.deletedCaption of
                    Nothing ->
                        text ""

                    _ ->
                        button [ onClick UndoDeletion ]
                            [ text "Undo Deletion" ]
                ]
            ]
        , tr []
            [ td [ colspan 2 ]
                [ textarea
                    [ disabled isDisabled
                    , rows 4
                    , cols 50
                    , style "font-size" "20px"
                    , onInput SetText
                    , id "text"
                    , value inputs.text
                    ]
                    []
                ]
            ]
        , tr []
            [ th "Position:"
            , td []
                [ positionSelector isDisabled model ]
            ]
        , tr []
            [ th "Text Alignment:"
            , td []
                [ alignmentSelector isDisabled model ]
            ]
        , tr []
            [ th "Font:"
            , td []
                [ fontSelector isDisabled model ]
            ]
        , tr []
            [ th "Font Height:"
            , td []
                [ input
                    [ type_ "text"
                    , disabled isDisabled
                    , textalign "right"
                    , size 3
                    , onInput SetFontSize
                    , value inputs.fontsize
                    ]
                    []
                , text "%"
                ]
            ]
        , tr []
            [ th "Font Color:"
            , td []
                [ colorSelector isDisabled inputs.fontcolor
                , text " "
                , input
                    [ type_ "text"
                    , disabled isDisabled
                    , size 20
                    , onInput SetFontColor
                    , value inputs.fontcolor
                    ]
                    []
                ]
            ]
        , tr []
            [ th "Bold:"
            , td []
                [ input
                    [ type_ "checkbox"
                    , disabled isDisabled
                    , onCheck SetBold
                    , checked inputs.bold
                    ]
                    []
                ]
            ]
        , tr []
            [ th "Width:"
            , td []
                [ input
                    [ type_ "text"
                    , disabled isDisabled
                    , textalign "right"
                    , size 3
                    , onInput SetWidth
                    , value inputs.width
                    ]
                    []
                , text "%"
                ]
            ]
        , tr []
            [ th "Height:"
            , td []
                [ input
                    [ type_ "text"
                    , disabled isDisabled
                    , textalign "right"
                    , size 3
                    , onInput SetHeight
                    , value inputs.height
                    ]
                    []
                , text "%"
                ]
            ]
        ]


allPositions : List TextPosition
allPositions =
    [ TopCenter
    , MiddleCenter
    , BottomCenter
    , TopLeft
    , TopRight
    , MiddleLeft
    , MiddleRight
    , BottomLeft
    , BottomRight
    ]


positionSelector : Bool -> Model -> Html Msg
positionSelector isDisabled model =
    let
        position =
            model.selectedPosition

        captions =
            model.meme.captions

        positions =
            List.filter
                (\p ->
                    (position == Just p)
                        || (Nothing == LE.find (\c -> p == c.position) captions)
                )
                allPositions
    in
    select
        [ disabled isDisabled
        , onInput SetPositionString
        ]
    <|
        List.map (positionOption position) positions


positionOption : Maybe TextPosition -> TextPosition -> Html Msg
positionOption currentPosition position =
    let
        positionString =
            textPositionToString position
    in
    option
        [ selected <| currentPosition == Just position
        , value positionString
        ]
        [ text <| textPositionToString position ]


alignmentSelector : Bool -> Model -> Html Msg
alignmentSelector isDisabled model =
    let
        currentAlignment =
            case findCaption model.selectedPosition model.meme.captions of
                Nothing ->
                    Center

                Just caption ->
                    caption.alignment
    in
    select
        [ disabled isDisabled
        , onInput SetAlignmentString
        ]
    <|
        List.map (alignmentOption currentAlignment) allAlignments


alignmentOption : TextAlignment -> TextAlignment -> Html Msg
alignmentOption currentAlignment alignment =
    let
        alignmentString =
            alignmentToString alignment
    in
    option
        [ selected <| currentAlignment == alignment
        , value alignmentString
        ]
        [ text alignmentString ]


colors : List String
colors =
    [ "black"
    , "white"
    , "red"
    , "green"
    , "blue"
    , "purple"
    , "orange"
    , "yellow"
    , "turquoise"
    ]


colorSelector : Bool -> String -> Html Msg
colorSelector isDisabled color =
    select
        [ disabled isDisabled
        , onInput SetFontColor
        ]
    <|
        customColorOption color
            :: List.map
                (colorOption color)
                colors


customColorOption : String -> Html Msg
customColorOption currentColor =
    let
        isCustom =
            not <| List.member currentColor colors
    in
    option
        [ selected isCustom
        , value ""
        ]
        [ text "Custom" ]


colorOption : String -> String -> Html Msg
colorOption currentColor color =
    option
        [ selected <| currentColor == color
        , value color
        ]
        [ text color ]


fontSelector : Bool -> Model -> Html Msg
fontSelector isDisabled model =
    select
        [ disabled isDisabled
        , onInput SetFont
        ]
    <|
        List.map (fontOption model.inputs.font)
            (Dict.toList model.fontDict
                |> List.map Tuple.second
            )


fontOption : String -> Font -> Html Msg
fontOption currentFont font =
    option
        [ selected <| currentFont == font.font
        , value font.font
        ]
        -- font attributes don't work here in most browsers
        [ span [ fontAttribute font ]
            [ text font.font ]
        ]


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


renderMeme : Model -> Html Msg
renderMeme model =
    let
        meme =
            model.meme

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
                    [ width w
                    , height h
                    , xlinkHref url
                    , Svg.Events.onClick <| SelectCaption Nothing
                    ]
                    []
              ]
            , List.map (renderCaption model) meme.captions
            ]


tos : Int -> String
tos int =
    String.fromInt int


renderCaption : Model -> Caption -> Svg Msg
renderCaption model caption =
    let
        meme =
            model.meme

        isSelected =
            Just caption.position == model.selectedPosition

        ( ( cx, cy ), ( cw, ch ) ) =
            captionCoordinates caption meme.width meme.height

        showCaptionBorders =
            model.showCaptionBorders || isSelected

        alignment =
            alignmentToString caption.alignment
                |> String.toLower

        font =
            Maybe.withDefault defaultFont <| Dict.get caption.font model.fontDict

        fontsize =
            caption.fontsize * meme.height // 100

        weight =
            if caption.bold then
                "bold"

            else
                "normal"

        stroke =
            if isSelected then
                "stroke:red;"

            else
                "stroke:black;"

        strokeOpacity =
            if showCaptionBorders then
                "stroke-opacity:1;"

            else
                "stroke-opacity:0;"

        dashArray =
            if isSelected then
                "8,5"

            else
                "2,2"

        rectStyle =
            "fill-opacity:0;stroke-width:2;" ++ stroke ++ strokeOpacity
    in
    Svg.g []
        [ foreignObject
            [ x (tos cx)
            , y (tos cy)
            , width (tos cw)
            , height (tos ch)
            ]
            [ div
                [ textalign alignment
                , fontAttribute font
                , style "font-size" <| tos fontsize ++ "px"
                , style "color" caption.fontcolor
                , style "font-weight" weight
                ]
              <|
                splitLines caption.text
            ]
        , rect
            [ x (tos cx)
            , y (tos cy)
            , width (tos cw)
            , height (tos ch)
            , strokeDasharray dashArray
            , Svg.Attributes.style rectStyle
            , Svg.Events.onClick <| SelectCaption (Just caption.position)
            ]
            []
        ]


splitLines : String -> List (Html msg)
splitLines string =
    String.split "<br>" string
        |> List.map text
        |> List.intersperse br


codestr code =
    String.fromList [ Char.fromCode code ]


chars =
    { leftCurlyQuote = codestr 0x201C
    , copyright = codestr 0xA9
    , nbsp = codestr 0xA0
    }
