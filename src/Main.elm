---------------------------------------------------------------------
--
-- Main.elm
-- Zap Meme top-level
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
import Bytes exposing (Bytes)
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import CustomElement.ImageProperties as ImageProperties exposing (ImageProperties)
import CustomElement.SvgToDataUrl as SvgToDataUrl exposing (ReturnedFile, ReturnedUrl)
import Dict exposing (Dict)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)
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
import Markdown
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
        , strokeWidth
        , textAnchor
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
import Svg.Button as SB exposing (Button, Content(..))
import Svg.Events
import Svg.Lazy
import Task
import Time exposing (Posix)
import Url exposing (Url)
import ZapMeme.Data exposing (data)
import ZapMeme.EncodeDecode as ED
import ZapMeme.Types
    exposing
        ( Caption
        , Font
        , Image
        , Meme
        , TextAlignment(..)
        , TextPosition(..)
        )


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


initialImage =
    { url = data.pigeon
    }


sampleCaptions : List Caption
sampleCaptions =
    [ { text = "I ask you<br>once again:"
      , position = TopCenter
      , alignment = Center
      , font = "impact"
      , fontsize = 10
      , fontcolor = "white"
      , outlineColor = Nothing
      , bold = True
      , width = 75
      , height = 30
      }
    , { text = "Is this a pigeon?"
      , position = BottomCenter
      , alignment = Center
      , font = "impact"
      , fontsize = 10
      , fontcolor = "white"
      , outlineColor = Nothing
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


type ButtonOperation
    = IncrementButton
    | DecrementButton


type alias Model =
    { meme : Meme
    , selectedPosition : Maybe TextPosition
    , deletedCaption : Maybe Caption
    , showCaptionBorders : Bool
    , controller : Controller
    , inputs : Inputs
    , maxWidth : Int
    , maxHeight : Int
    , file : Maybe File
    , triggerImageProperties : Int
    , downloadFile : Maybe File
    , triggerReturnedFile : Int
    , fileName : String
    , mimeType : String
    , incrementButton : Button ()
    , decrementButton : Button ()
    , showHelp : Bool
    , subscription : Maybe Subscription
    , fontDict : Dict String Font
    , key : Key
    , funnelState : PortFunnels.State
    , msg : Maybe String
    }


type alias Subscription =
    { delay : Int
    , millis : Int
    , buttonMsg : SB.Msg
    , operation : ButtonOperation
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


initialInputs : Inputs
initialInputs =
    { text = ""
    , imageUrl = ""
    , position = TopCenter
    , alignment = Center
    , font = "avante-garde"
    , fontsize = "10"
    , fontcolor = "white"
    , isOutlined = False
    , outlineColor = "black"
    , bold = True
    , width = "75"
    , height = "15"
    , fileName = "meme"
    }


type Controller
    = MaxWidthController
    | MaxHeightController
    | FontHeightController
    | CaptionWidthController
    | CaptionHeightController


currentCaptionAccessors : ( Caption -> Float, Float -> Caption -> Caption ) -> (Float -> Inputs -> Inputs) -> ( Model -> Float, Float -> Model -> Model )
currentCaptionAccessors ( reader, writer ) inputsUpdater =
    let
        cr model =
            case findCaption model.selectedPosition model.meme.captions of
                Nothing ->
                    0

                Just c ->
                    reader c

        cw x model =
            case findCaption model.selectedPosition model.meme.captions of
                Nothing ->
                    model

                Just _ ->
                    let
                        inputs =
                            model.inputs
                    in
                    { model
                        | inputs = inputsUpdater x inputs
                    }
                        |> updateCaption (\c -> writer x c)
    in
    ( cr, cw )


getControllerAccessors : Controller -> ( Model -> Float, Float -> Model -> Model )
getControllerAccessors controller =
    case controller of
        MaxWidthController ->
            ( .maxWidth >> toFloat, \x m -> { m | maxWidth = round x } )

        MaxHeightController ->
            ( .maxHeight >> toFloat, \x m -> { m | maxHeight = round x } )

        FontHeightController ->
            currentCaptionAccessors
                ( .fontsize, \x c -> { c | fontsize = x } )
                (\x c -> { c | fontsize = ftos x })

        CaptionWidthController ->
            currentCaptionAccessors
                ( .width >> toFloat, \x c -> { c | width = round x } )
                (\x c -> { c | width = tos <| round x })

        CaptionHeightController ->
            currentCaptionAccessors
                ( .height >> toFloat, \x c -> { c | height = round x } )
                (\x c -> { c | height = tos <| round x })


type Msg
    = Noop
    | InitialDelay Posix
    | Tick Posix
    | SelectCaption (Maybe TextPosition)
    | AddCaption
    | DeleteCaption
    | UndoDeletion
    | ToggleHelp
    | SetShowCaptionBorders Bool
    | SetText String
    | SelectImageFile
    | ReceiveImageFile File
    | ReceiveImageUrl String
    | ReceiveImageProperties ImageProperties
    | GetReturnedFile
    | ReceiveReturnedFile ReturnedFile
    | ReceiveReturnedBytes Bytes
    | SetImageUrl String
    | SetMemeImageUrl
    | SetController Controller Bool
    | ButtonMsg SB.Msg ButtonOperation
    | SetMaxWidth String
    | SetMaxHeight String
    | SetPosition TextPosition
    | SetPositionString String
    | SetAlignment TextAlignment
    | SetAlignmentString String
    | SetFont String
    | SetFontSize String
    | SetFontColor String
    | SetOutlineColor String
    | SetBold Bool
    | SetWidth String
    | SetHeight String
    | SetFileName String
    | StartDownload String String
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


shortRepeatTimeDelay : Float
shortRepeatTimeDelay =
    100



-- This is the default, but I want to be explicit


repeatTime : SB.RepeatTime
repeatTime =
    SB.RepeatTimeWithInitialDelay 500 shortRepeatTimeDelay


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every shortRepeatTimeDelay Tick
        , PortFunnels.subscriptions ProcessLocalStorage model
        ]


localStoragePrefix : String
localStoragePrefix =
    "elm-meme-maker"


buttonSize : Float
buttonSize =
    40


buttonPair : ( Float, Float )
buttonPair =
    ( buttonSize, buttonSize )


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    { meme = emptyMeme
    , deletedCaption = Nothing
    , selectedPosition = Nothing
    , showCaptionBorders = False
    , controller = CaptionHeightController
    , inputs = initialInputs
    , maxWidth = 800
    , maxHeight = 600
    , file = Nothing
    , triggerImageProperties = 0
    , downloadFile = Nothing
    , triggerReturnedFile = 0
    , fileName = "mime.jpg"
    , mimeType = "image/jpeg"
    , incrementButton =
        SB.repeatingButton repeatTime
            buttonPair
            ()
    , decrementButton =
        SB.repeatingButton repeatTime
            buttonPair
            ()
    , showHelp = False
    , subscription = Nothing
    , fontDict = safeFontDict
    , key = key
    , funnelState = PortFunnels.initialState localStoragePrefix
    , msg = Nothing
    }
        |> withCmd
            -- Need to do this so we compute the size at startup
            (Task.perform ReceiveImageUrl <|
                Task.succeed initialImage.url
            )


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
                        , imageUrl = model.meme.image.url
                        , position = caption.position
                        , alignment = caption.alignment
                        , font = caption.font
                        , fontsize = fontFormat caption.fontsize
                        , fontcolor = caption.fontcolor
                        , isOutlined = caption.outlineColor /= Nothing
                        , outlineColor =
                            case caption.outlineColor of
                                Just color ->
                                    color

                                Nothing ->
                                    model.inputs.outlineColor
                        , bold = caption.bold
                        , width = tos caption.width
                        , height = tos caption.height
                        , fileName = model.inputs.fileName
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


toi : Int -> String -> Int
toi default string =
    Maybe.withDefault default <| String.toInt string


tof : Float -> String -> Float
tof default string =
    Maybe.withDefault default <| String.toFloat string


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
                inputs =
                    model.inputs

                caption =
                    { text = inputs.text
                    , position = position
                    , alignment = inputs.alignment
                    , font = inputs.font
                    , fontsize = tof 10 inputs.fontsize
                    , fontcolor = "white"
                    , outlineColor =
                        if inputs.isOutlined then
                            Just inputs.outlineColor

                        else
                            Nothing
                    , bold = True
                    , width = toi 75 inputs.width
                    , height = toi 15 inputs.height
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

        InitialDelay posix ->
            case model.subscription of
                Nothing ->
                    model |> withNoCmd

                Just subscription ->
                    { model
                        | subscription =
                            Just
                                { subscription
                                    | millis =
                                        subscription.delay
                                            + Time.posixToMillis posix
                                }
                    }
                        |> withNoCmd

        Tick posix ->
            case model.subscription of
                Nothing ->
                    model |> withNoCmd

                Just { millis, buttonMsg, operation } ->
                    if millis >= 0 && millis <= Time.posixToMillis posix then
                        ( model
                        , Task.perform (ButtonMsg buttonMsg) <|
                            Task.succeed operation
                        )

                    else
                        model |> withNoCmd

        AddCaption ->
            addCaption model

        DeleteCaption ->
            deleteCaption model

        UndoDeletion ->
            undoDeletion model

        ToggleHelp ->
            { model | showHelp = not model.showHelp }
                |> withNoCmd

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

        SelectImageFile ->
            model |> withCmd (Select.file imageMimeTypes ReceiveImageFile)

        ReceiveImageFile file ->
            { model | file = Just file }
                |> withCmd (Task.perform ReceiveImageUrl <| File.toUrl file)

        ReceiveImageUrl url ->
            receiveImageUrl url model

        ReceiveImageProperties properties ->
            if properties.width < 0 || properties.height < 0 then
                model |> withNoCmd

            else
                let
                    meme =
                        model.meme
                in
                { model
                    | meme =
                        { meme
                            | width = properties.width
                            , height = properties.height
                        }
                }
                    |> withNoCmd

        StartDownload mimeType extension ->
            { model
                | mimeType = mimeType
                , fileName = inputs.fileName ++ extension
            }
                |> withCmd (Task.perform (\_ -> GetReturnedFile) Time.now)

        GetReturnedFile ->
            { model | triggerReturnedFile = model.triggerReturnedFile + 1 }
                |> withNoCmd

        ReceiveReturnedFile returnedFile ->
            let
                downloadFile =
                    returnedFile.file
            in
            { model | downloadFile = Just downloadFile }
                |> withCmd
                    (Task.perform ReceiveReturnedBytes <|
                        File.toBytes downloadFile
                    )

        ReceiveReturnedBytes bytes ->
            case model.downloadFile of
                Nothing ->
                    model |> withNoCmd

                Just file ->
                    { model | file = Nothing }
                        |> withCmd
                            (Download.bytes (File.name file)
                                (File.mime file)
                                bytes
                            )

        SetImageUrl string ->
            { model
                | inputs = { inputs | imageUrl = string }
            }
                |> withNoCmd

        SetMemeImageUrl ->
            let
                meme =
                    model.meme

                image =
                    meme.image
            in
            { model
                | meme =
                    { meme
                        | image =
                            { image | url = inputs.imageUrl }
                    }
            }
                |> withNoCmd

        SetController controller checked ->
            (if checked then
                { model | controller = controller }

             else
                model
            )
                |> withNoCmd

        -- Process a msg for Svg.Button
        ButtonMsg m operation ->
            let
                button =
                    case operation of
                        IncrementButton ->
                            model.incrementButton

                        DecrementButton ->
                            model.decrementButton

                wrapper =
                    \bm -> ButtonMsg bm operation
            in
            case SB.checkSubscription (Debug.log "checkSubscription" m) button of
                Just ( time, m2 ) ->
                    let
                        ( subscription, cmd ) =
                            if time <= 0 then
                                ( Nothing, Cmd.none )

                            else if time == shortRepeatTimeDelay then
                                ( Just
                                    { delay = 0
                                    , millis = 0
                                    , buttonMsg = m2
                                    , operation = operation
                                    }
                                , Cmd.none
                                )

                            else
                                ( Just
                                    { delay = round time
                                    , millis = -1
                                    , buttonMsg = m2
                                    , operation = operation
                                    }
                                , Task.perform InitialDelay Time.now
                                )
                    in
                    { model
                        | subscription = subscription
                    }
                        |> withCmd cmd

                Nothing ->
                    let
                        ( isClick, btn, cmd ) =
                            SB.update wrapper m button

                        mdl =
                            case operation of
                                IncrementButton ->
                                    { model | incrementButton = btn }

                                DecrementButton ->
                                    { model | decrementButton = btn }
                    in
                    buttonOperate isClick operation mdl
                        |> withCmd cmd

        SetMaxWidth string ->
            { model
                | maxWidth =
                    toi model.maxWidth string
            }
                |> withNoCmd

        SetMaxHeight string ->
            { model
                | maxHeight =
                    toi model.maxHeight string
            }
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
                |> updateCaption (\c -> { c | fontsize = tof 10 string })
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

        SetOutlineColor string ->
            let
                color =
                    if string == "none" || string == "" then
                        inputs.outlineColor

                    else
                        string

                isOutlined =
                    string /= "none"

                outlineColor =
                    if isOutlined then
                        Just color

                    else
                        Nothing
            in
            { model
                | inputs =
                    { inputs
                        | outlineColor = color
                        , isOutlined = isOutlined
                    }
            }
                |> updateCaption (\c -> { c | outlineColor = outlineColor })
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

        SetFileName name ->
            { model
                | inputs =
                    { inputs | fileName = name }
            }
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


buttonOperate : Bool -> ButtonOperation -> Model -> Model
buttonOperate isClick operation model =
    if not isClick then
        model

    else
        let
            ( reader, writer ) =
                getControllerAccessors model.controller

            increment =
                case operation of
                    IncrementButton ->
                        1.0

                    DecrementButton ->
                        -1.0
        in
        writer (reader model + increment) model


receiveImageUrl : String -> Model -> ( Model, Cmd Msg )
receiveImageUrl url model =
    let
        meme =
            model.meme

        image =
            meme.image
    in
    { model
        | meme =
            { meme
                | image = { image | url = url }
            }
        , triggerImageProperties = model.triggerImageProperties + 1
    }
        |> withNoCmd


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


b : String -> Html msg
b string =
    Html.b [] [ text string ]


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


type alias ScaleWH =
    { width : Int
    , height : Int
    , scale : Float
    }


identityScale : Model -> ScaleWH
identityScale model =
    let
        meme =
            model.meme
    in
    ScaleWH meme.width meme.height 1.0


scalewh : ( Int, Int ) -> ( Int, Int ) -> ScaleWH
scalewh ( maxw, maxh ) ( w, h ) =
    let
        fw =
            toFloat w

        fh =
            toFloat h

        wscale =
            toFloat (max 100 maxw) / fw

        hscale =
            toFloat (max 100 maxh) / fh

        scale =
            min 1 (min wscale hscale)
    in
    ScaleWH (round <| scale * fw) (round <| scale * fh) scale


style_ : String -> Html msg
style_ string =
    Html.node "style" [ type_ "text/css" ] [ text string ]


center =
    Html.node "center"


view : Model -> Document Msg
view model =
    let
        meme =
            model.meme

        scale =
            scalewh ( model.maxWidth, model.maxHeight ) ( meme.width, meme.height )
    in
    { title = "ZAP Meme"
    , body =
        [ div
            [ align "center" ]
            [ h2 [] [ text "ZAP Meme" ]
            , p []
                [ renderMeme scale model
                , ImageProperties.imageProperties
                    [ ImageProperties.imageId imageId
                    , ImageProperties.triggerImageProperties
                        model.triggerImageProperties
                    , ImageProperties.onImageProperties ReceiveImageProperties
                    ]
                    []
                , SvgToDataUrl.svgToDataUrl
                    [ SvgToDataUrl.returnedFileParameters svgId
                        model.fileName
                        model.mimeType
                    , SvgToDataUrl.triggerReturnedFile model.triggerReturnedFile
                    , SvgToDataUrl.onReturnedFile ReceiveReturnedFile
                    ]
                    []
                ]
            , p []
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
                , renderInputs scale model
                ]
            , button
                [ onClick ToggleHelp
                ]
                [ text <|
                    if model.showHelp then
                        "Hide Help"

                    else
                        "Show Help"
                ]
            , if model.showHelp then
                helpParagraph

              else
                text ""
            , fontParagraph
            , p []
                [ text <| chars.copyright ++ " 2019 Bill St. Clair"
                , br
                , a
                    [ href "https://github.com/billstclair/zapmeme"
                    , target "_blank"
                    ]
                    [ text "GitHub" ]
                ]
            ]
        ]
    }


thm : List (Html msg) -> Html msg
thm body =
    Html.th
        [ style "vertical-align" "top"
        , textalign "right"
        ]
        body


th : String -> Html msg
th string =
    thm [ text string ]


textalign : String -> Attribute msg
textalign string =
    style "text-align" string


svgalign : TextAlignment -> Svg.Attribute msg
svgalign alignment =
    textAnchor <|
        case alignment of
            Left ->
                "start"

            Right ->
                "end"

            Center ->
                "middle"


controllerRadio : Bool -> Controller -> Model -> Html Msg
controllerRadio isDisabled controller model =
    input
        [ type_ "radio"
        , name "controller"
        , disabled isDisabled
        , onCheck <| SetController controller
        , checked <| model.controller == controller
        ]
        []


renderInputs : ScaleWH -> Model -> Html Msg
renderInputs scale model =
    let
        inputs =
            model.inputs

        isDisabled =
            model.selectedPosition == Nothing

        meme =
            model.meme
    in
    table []
        [ tr []
            [ td [ colspan 2 ]
                [ table []
                    [ tr []
                        [ td []
                            [ textarea
                                [ rows 3
                                , cols 50
                                , style "font-size" "20px"
                                , onInput SetText
                                , id "text"
                                , value inputs.text
                                ]
                                []
                            ]
                        , td []
                            [ svg
                                [ style "margin" "auto"
                                , height <| ftos (2 * buttonSize - 2)
                                , width <| ftos buttonSize
                                ]
                                [ SB.render ( 0, 0 )
                                    (SB.TextContent "^")
                                    (\m -> ButtonMsg m IncrementButton)
                                    model.incrementButton
                                , SB.render ( 0, buttonSize - 2 )
                                    (SB.TextContent "v")
                                    (\m -> ButtonMsg m DecrementButton)
                                    model.decrementButton
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , tr []
            [ td
                [ colspan 2
                , style "text-align" "center"
                ]
                [ b "Selected Caption" ]
            ]
        , tr []
            [ thm
                [ controllerRadio isDisabled CaptionWidthController model
                , text " Width:"
                ]
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
                , text "% "
                , controllerRadio isDisabled CaptionHeightController model
                , b " Height: "
                , input
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
        , tr []
            [ th "Position:"
            , td []
                [ positionSelector isDisabled model
                , b " Borders: "
                , input
                    [ type_ "checkbox"
                    , onCheck SetShowCaptionBorders
                    , checked model.showCaptionBorders
                    ]
                    []
                ]
            ]
        , tr []
            [ th "Alignment:"
            , td []
                [ alignmentSelector isDisabled model
                , b " Bold: "
                , input
                    [ type_ "checkbox"
                    , disabled isDisabled
                    , onCheck SetBold
                    , checked inputs.bold
                    ]
                    []
                ]
            ]
        , tr []
            [ th "Font:"
            , td []
                [ fontSelector isDisabled model
                , text " "
                , controllerRadio isDisabled FontHeightController model
                , b " Height: "
                , input
                    [ type_ "text"
                    , disabled isDisabled
                    , textalign "right"
                    , size 5
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
                [ colorSelector False False isDisabled inputs.fontcolor
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
            [ th "Outline Color:"
            , let
                isOutlined =
                    inputs.isOutlined

                color =
                    if isOutlined then
                        inputs.outlineColor

                    else
                        "none"
              in
              td []
                [ colorSelector True (not isOutlined) isDisabled inputs.outlineColor
                , text " "
                , input
                    [ type_ "text"
                    , disabled (isDisabled || not isOutlined)
                    , size 20
                    , onInput SetOutlineColor
                    , value color
                    ]
                    []
                , if isOutlined then
                    span []
                        [ br
                        , text "(not line-wrapped, use <br>)"
                        ]

                  else
                    text ""
                ]
            ]
        , tr []
            [ td
                [ colspan 2
                , style "text-align" "center"
                ]
                [ b "Background" ]
            ]
        , tr []
            [ th "Image:"
            , td []
                [ button [ onClick <| ReceiveImageUrl initialImage.url ]
                    [ text "Use Default" ]
                , text " "
                , button [ onClick SelectImageFile ]
                    [ text "Choose File" ]
                , br
                , input
                    [ type_ "text"
                    , size 20
                    , onInput SetImageUrl
                    , value inputs.imageUrl
                    ]
                    []
                , button [ onClick SetMemeImageUrl ]
                    [ text "Use URL" ]
                ]
            ]
        , tr []
            [ thm
                [ controllerRadio False MaxWidthController model
                , text " Max Width:"
                ]
            , td []
                [ input
                    [ type_ "text"
                    , size 5
                    , onInput SetMaxWidth
                    , value <| tos model.maxWidth
                    ]
                    []
                , text " "
                , controllerRadio False MaxHeightController model
                , b " Height: "
                , input
                    [ type_ "text"
                    , size 5
                    , onInput SetMaxHeight
                    , value <| tos model.maxHeight
                    ]
                    []
                , br
                , span [ style "font-size" "80%" ]
                    [ text "("
                    , text <| tos meme.width
                    , text " x "
                    , text <| tos meme.height
                    , text ") * "
                    , text <| format usLocale scale.scale
                    , text " = ("
                    , text <| tos scale.width
                    , text " x "
                    , text <| tos scale.height
                    , text ")"
                    ]
                ]
            ]
        , tr []
            [ th "File Name:"
            , td []
                [ input
                    [ type_ "text"
                    , size 20
                    , onInput SetFileName
                    , value inputs.fileName
                    ]
                    []
                , b " Download: "
                , button [ onClick <| StartDownload "image/jpeg" ".jpg" ]
                    [ text "JPEG" ]
                , text " "
                , button [ onClick <| StartDownload "image/png" ".png" ]
                    [ text "PNG" ]
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


colorSelector : Bool -> Bool -> Bool -> String -> Html Msg
colorSelector includeNone isNone isDisabled color =
    let
        msg =
            if includeNone then
                SetOutlineColor

            else
                SetFontColor

        currentNone =
            if isNone then
                "none"

            else
                ""

        isCustom =
            not isNone
                && (not <| List.member color colors)

        opts =
            customColorOption isCustom color
                :: List.map
                    (colorOption isNone color)
                    colors

        options =
            if includeNone then
                colorOption False currentNone "none"
                    :: opts

            else
                opts
    in
    select
        [ disabled isDisabled
        , onInput msg
        ]
    <|
        options


customColorOption : Bool -> String -> Html Msg
customColorOption isSelected currentColor =
    let
        isCustom =
            not <| List.member currentColor colors
    in
    option
        [ selected isSelected
        , value ""
        , disabled (not isCustom && not isSelected)
        ]
        [ text "Custom" ]


colorOption : Bool -> String -> String -> Html Msg
colorOption isNone currentColor color =
    option
        [ selected <| not isNone && (currentColor == color)
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


helpParagraph : Html msg
helpParagraph =
    Markdown.toHtml
        [ style "width" "60%"
        , style "text-align" "left"
        ]
        """
You will usually want to choose an image other than the default from the
"Background" section. "Max Width" and its "Height" are the maximum
background image size, in pixels. The image will be scaled to fit. The
"JPEG" and "PNG" download buttons will save to you computer an image
of the completed meme in "File Name", with suitable extension.

To add a new caption, click "Add Caption". This will use the text in
the text input area, just below the "Add Caption" button, a new
"Position", not occupied by any existing captions, and the other
settings below "Selected Caption".

To change an existing caption, click on it. It will be outlined with a
red, dashed line, and the editing controls will be enabled. To delete
the selected caption, click "Delete Caption" (which is not there
unless a caption is selected). You can undo that, but only until you
select or add another caption.

Typing in the text input area, right below the meme, changes the
caption's text. It will be auto-wrapped, unless you choose an "Outline
Color" (which I will fix soon). To insert an explicit line break,
enter "<br>".

"Width" and "Height" change the size of the outlined area. They are
represented in percentages of the background image width and height.
The easiest way to change any numeric field, is to click the radio
button beside it, and click the up or down array to the right of the
text input area. Those arrow buttons will repeat if you hold them
down.

"Position" lets you select one of nine positions for a caption.

"Alignment" controls where in the outlined rectangle the text will appear.

If "Bold" is checked, most, but not all fonts, will look thicker.

"Font" is one of a selection of safe web fonts, which usually work in
any browser. I plan to extend this, enabling fonts from Google and
others, and allowing you to show a subset of the available fonts, if
you don't expect to use some of them.

"Height" is the height of the font, in percentage of the image
height. The arrow buttons change this by 1, but you can type a single
decimal digit, if you need fine tuning (e.g. "7.5").

"Font Color" provides a list of choices, but you can type any HTML
font in the text area to its right. E.g. "#8f0" for a bright green (a
little red mixed in).

If you choose an "Outline Color", the text will stand out better from
the background image. Again, you may enter a custom color.

Next feature: Save the current meme, and allow saving of multiple
other memes and "Selected Caption" settings, named as you like to help
you remember them.
         """


fontParagraph : Html msg
fontParagraph =
    p [ style "width" "80%" ] <|
        List.concat
            [ [ span
                    [ style "font-size" "110%"
                    , style "font-weight" "bold"
                    ]
                    [ text "Fonts" ]
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


renderMeme : ScaleWH -> Model -> Html Msg
renderMeme scale model =
    let
        meme =
            model.meme

        image =
            meme.image

        wi =
            scale.width

        hi =
            scale.height

        selectedPosition =
            model.selectedPosition

        showCaptionBorders =
            model.showCaptionBorders

        fontDict =
            model.fontDict

        w =
            String.fromInt wi

        h =
            String.fromInt hi

        url =
            meme.image.url
    in
    svg
        [ Svg.Attributes.id svgId
        , width w
        , height h
        ]
    <|
        List.concat
            [ [ Svg.Lazy.lazy3 renderSvgImage wi hi url ]
            , let
                folder caption res =
                    let
                        svg =
                            Svg.Lazy.lazy6 renderCaption
                                wi
                                hi
                                selectedPosition
                                showCaptionBorders
                                fontDict
                                caption
                    in
                    svg :: res
              in
              List.foldl folder [] meme.captions
            ]


renderSvgImage : Int -> Int -> String -> Svg Msg
renderSvgImage wi hi url =
    Svg.image
        [ Svg.Attributes.id imageId
        , width <| tos wi
        , height <| tos hi
        , xlinkHref url
        , Svg.Events.onClick <| SelectCaption Nothing
        ]
        []


tos : Int -> String
tos int =
    String.fromInt int


ftos : Float -> String
ftos x =
    String.fromFloat x


renderCaption : Int -> Int -> Maybe TextPosition -> Bool -> Dict String Font -> Caption -> Svg Msg
renderCaption wi hi selectedPosition showCaptionBorders fontDict caption =
    let
        isSelected =
            Just caption.position == selectedPosition

        ( ( cx, cy ), ( cw, ch ) ) =
            captionCoordinates caption wi hi

        showBorders =
            showCaptionBorders || isSelected

        alignment =
            alignmentToString caption.alignment
                |> String.toLower

        font =
            Maybe.withDefault defaultFont <| Dict.get caption.font fontDict

        fontsize =
            round (caption.fontsize * toFloat hi / 100)

        weight =
            if caption.bold then
                "bold"

            else
                "normal"

        outline =
            if isSelected then
                "stroke:red;"

            else
                "stroke:black;"

        outlineOpacity =
            if showBorders then
                "stroke-opacity:1;"

            else
                "stroke-opacity:0;"

        dashArray =
            if isSelected then
                "8,5"

            else
                "2,2"

        rectStyle =
            "fill-opacity:0;stroke-width:2;" ++ outline ++ outlineOpacity
    in
    Svg.g []
        [ case caption.outlineColor of
            Nothing ->
                foreignObject
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

            Just outlineColor ->
                let
                    tx =
                        case caption.alignment of
                            Left ->
                                cx

                            Center ->
                                cx + cw // 2

                            Right ->
                                cx + cw

                    lines =
                        String.split "<br>" caption.text

                    max =
                        List.length lines - 1

                    -- This varies by font. Not doing that yet.
                    lineheight =
                        round (1.2 * toFloat fontsize)

                    ys =
                        List.map ((*) lineheight) (List.range 0 max)
                in
                g
                    [ transform
                        ("translate(" ++ tos tx ++ " " ++ (tos <| cy + fontsize) ++ ")")
                    , fontAttribute font
                    , style "font-size" <| tos fontsize ++ "px"
                    , fill caption.fontcolor
                    , stroke outlineColor
                    , strokeWidth "1"
                    , svgalign caption.alignment
                    ]
                <|
                    List.map2
                        (\txt ty ->
                            Svg.text_ [ y <| tos ty ]
                                [ Svg.text txt ]
                        )
                        lines
                        ys
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


imageMimeTypes : List String
imageMimeTypes =
    [ "image/png", "image/jpeg", "image/gif" ]


svgId : String
svgId =
    "svg"


imageId : String
imageId =
    "meme-image"


fontSizeLocale : Locale
fontSizeLocale =
    { usLocale | decimals = 1 }


fontFormat : Float -> String
fontFormat float =
    let
        res =
            format fontSizeLocale float
    in
    if String.endsWith ".0" res then
        String.dropRight 2 res

    else
        res
