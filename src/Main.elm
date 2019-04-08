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
import Dialog exposing (Config, Visible)
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
        , autofocus
        , checked
        , class
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
import MD5
import Markdown
import PortFunnel.LocalStorage as LocalStorage
import PortFunnels exposing (FunnelDict, Handler(..))
import Set exposing (Set)
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
        , Inputs
        , Meme
        , SavedModel
        , TextAlignment(..)
        , TextPosition(..)
        , WhichDialog(..)
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


emptyImage =
    { url = ""
    , hash = ""
    }


initialImage =
    { url = data.pigeon
    , hash = MD5.hex data.pigeon
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


initialMeme : Meme
initialMeme =
    { image = initialImage
    , captions = sampleCaptions
    , width = 839
    , height = 503
    }


emptyMeme : Meme
emptyMeme =
    { image = emptyImage
    , captions = []
    , width = 839
    , height = 503
    }


type ButtonOperation
    = IncrementButton
    | DecrementButton


type alias Model =
    { meme : Meme
    , dialog : WhichDialog
    , selectedPosition : Maybe TextPosition
    , savedSelectedPosition : Maybe TextPosition
    , showCaptionBorders : Bool
    , maxWidth : Int
    , maxHeight : Int
    , fileName : String
    , inputs : Inputs
    , showMemeImage : Bool
    , showHelp : Bool

    -- Below here is not persistent.
    , windowSize : ( Int, Int )
    , deletedCaption : Maybe Caption
    , controller : Controller
    , file : Maybe File
    , triggerImageProperties : Int
    , downloadFile : Maybe File
    , memeImageUrl : Maybe String
    , triggerReturnedFile : Int
    , triggerReturnedUrl : Int
    , mimeType : String
    , incrementButton : Button ()
    , decrementButton : Button ()
    , subscription : Maybe Subscription
    , fontDict : Dict String Font
    , key : Key
    , receiveImagesHandler : Maybe (String -> Maybe Value -> Cmd Msg)
    , receivedMeme : Maybe Meme
    , receivedModel : Maybe SavedModel
    , knownImages : Set String
    , images : Dict String Image
    , thumbnails : Dict String Image
    , thumbnailImageUrl : String
    , triggerThumbnailProperties : Int
    , neededThumbnails : List String
    , savedMemes : Set String
    , imageMemes : Dict String (List Meme)
    , funnelState : PortFunnels.State
    , msg : Maybe String
    }


type alias Subscription =
    { delay : Int
    , millis : Int
    , buttonMsg : SB.Msg
    , operation : ButtonOperation
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
    , savedMemeName = "meme"
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
    | WindowResize Int Int
    | SelectCaption (Maybe TextPosition)
    | AddCaption
    | DeleteCaption
    | UndoDeletion
    | ToggleHelp
    | SetDialog WhichDialog
    | SetImagesDialog
    | SetSavedMemeName String
    | SaveMeme
    | LoadSavedMeme String
    | DeleteSavedMeme String
    | DeleteSavedImage String
    | SetShowCaptionBorders Bool
    | SetText String
    | SelectImageFile
    | ReceiveImageFile File
    | ReceiveImageUrl String
    | ReceiveImageProperties ImageProperties
    | GetReturnedFile
    | ReceiveReturnedFile ReturnedFile
    | ReceiveReturnedBytes Bytes
    | ReceiveMemeImageUrl String
    | ShowMemeImage Bool String
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
    | MaybePutImageUrl String String
    | ImageExists String
    | PutImageUrl String String
    | ReceiveImageKey String (Maybe Value)
    | ReceiveMeme (Maybe Value)
    | ReceiveImage String (Maybe Value)
    | ReceiveModel (Maybe Value)
    | ReceiveMemeKeys (List String)
    | ReceiveImageUrls (List String)
    | ReceiveThumbnail String (Maybe Value)
    | ReceiveImageForThumbnail String (Maybe Value)
    | ReceiveThumbnailProperties ImageProperties
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
        , Events.onResize WindowResize
        , PortFunnels.subscriptions ProcessLocalStorage model
        ]


buttonSize : Float
buttonSize =
    40


buttonPair : ( Float, Float )
buttonPair =
    ( buttonSize, buttonSize )


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { meme = emptyMeme
            , dialog = NoDialog
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
            , memeImageUrl = Nothing
            , triggerReturnedFile = 0
            , triggerReturnedUrl = 0
            , savedSelectedPosition = Nothing
            , fileName = "meme.jpg"
            , mimeType = "image/jpeg"
            , incrementButton =
                SB.repeatingButton repeatTime
                    buttonPair
                    ()
            , decrementButton =
                SB.repeatingButton repeatTime
                    buttonPair
                    ()
            , showMemeImage = False
            , showHelp = False
            , windowSize = ( 2000, 2000 )
            , subscription = Nothing
            , fontDict = safeFontDict
            , key = key
            , receiveImagesHandler = Nothing
            , receivedMeme = Nothing
            , receivedModel = Nothing
            , knownImages = Set.empty
            , images = Dict.empty
            , thumbnails = Dict.empty
            , thumbnailImageUrl = ""
            , triggerThumbnailProperties = 0
            , neededThumbnails = []
            , savedMemes = Set.empty
            , imageMemes = Dict.empty
            , funnelState = PortFunnels.initialState localStoragePrefix
            , msg = Nothing
            }
    in
    model
        |> withCmds
            [ Task.perform getViewport Dom.getViewport
            , get persistenceKeys.meme model
            , get persistenceKeys.model model
            , listKeys (persistenceKeys.memes ++ ".") model
            ]


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    WindowResize (round vp.width) (round vp.height)


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
                        , savedMemeName = model.inputs.savedMemeName
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
        ( mdl, cmd ) =
            updateInternal msg model

        doit =
            ((model.subscription /= Nothing)
                && (mdl.subscription == Nothing)
            )
                || ((mdl.subscription == Nothing)
                        && (case msg of
                                MaybePutImageUrl _ _ ->
                                    False

                                PutImageUrl _ _ ->
                                    False

                                ImageExists _ ->
                                    False

                                _ ->
                                    (modelToSavedModel mdl /= modelToSavedModel model)
                                        || (mdl.meme /= model.meme)
                           )
                   )
    in
    mdl
        |> withCmds
            [ cmd
            , if doit then
                putModel mdl

              else
                Cmd.none
            ]


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
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

        WindowResize w h ->
            { model | windowSize = ( w, h ) }
                |> withNoCmd

        AddCaption ->
            addCaption model

        DeleteCaption ->
            deleteCaption model

        UndoDeletion ->
            undoDeletion model

        ToggleHelp ->
            { model | showHelp = not model.showHelp }
                |> withNoCmd

        SetDialog dialog ->
            { model | dialog = dialog }
                |> withNoCmd

        SetImagesDialog ->
            { model | dialog = ImagesDialog }
                |> withCmd (listImages model)

        SetSavedMemeName name ->
            { model | inputs = { inputs | savedMemeName = name } }
                |> withNoCmd

        SaveMeme ->
            { model
                | dialog = NoDialog
                , savedMemes =
                    Set.insert inputs.savedMemeName model.savedMemes
            }
                |> withCmd (putSavedMeme inputs.savedMemeName (Just model.meme) model)

        LoadSavedMeme name ->
            { model | savedMemes = Set.insert name model.savedMemes }
                |> withCmd (getSavedMeme name model)

        DeleteSavedMeme name ->
            { model | savedMemes = Set.remove name model.savedMemes }
                |> withCmd (putSavedMeme name Nothing model)

        DeleteSavedImage hash ->
            -- TODO
            model |> withNoCmd

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

        MaybePutImageUrl hash url ->
            if Set.member hash model.knownImages then
                model |> withNoCmd

            else
                let
                    flagKey =
                        Debug.log "MaybePutImageUrl" <|
                            encodeSubkey persistenceKeys.images hash
                in
                { model | receiveImagesHandler = Just <| receivePutImageImages url }
                    |> withCmd (get flagKey model)

        ImageExists hash ->
            { model
                | knownImages = Set.insert hash model.knownImages
            }
                |> withNoCmd

        PutImageUrl hash url ->
            let
                flagKey =
                    encodeSubkey persistenceKeys.images hash

                key =
                    Debug.log "PutImageUrl" <|
                        encodeSubkey persistenceKeys.imageurls hash
            in
            { model
                | knownImages = Set.insert hash model.knownImages
            }
                |> withCmds
                    [ put flagKey (Just <| JE.bool True) model
                    , put key (Just <| JE.string url) model
                    ]

        ReceiveImageKey hash value ->
            case model.receiveImagesHandler of
                Nothing ->
                    model |> withNoCmd

                Just handler ->
                    ( { model | receiveImagesHandler = Nothing }
                    , handler hash value
                    )

        ReceiveMeme value ->
            case Debug.log "ReceiveMeme" value of
                Nothing ->
                    useInitialMeme model

                Just v ->
                    case ED.decodeMeme v of
                        Err _ ->
                            useInitialMeme model

                        Ok meme ->
                            ( { model | receivedMeme = Just meme }
                            , get
                                (encodeSubkey persistenceKeys.imageurls
                                    meme.image.hash
                                )
                                model
                            )

        ReceiveImage hash value ->
            let
                mdl1 =
                    { model
                        | receivedMeme = Nothing
                        , receivedModel = Nothing
                    }

                mdl =
                    case model.receivedModel of
                        Nothing ->
                            mdl1

                        Just savedModel ->
                            savedModelToModel savedModel mdl1
            in
            case Debug.log "ReceiveImage" value of
                Nothing ->
                    useInitialMeme mdl

                Just v ->
                    case model.receivedMeme of
                        Nothing ->
                            useInitialMeme mdl

                        Just meme ->
                            case JD.decodeValue JD.string v of
                                Err _ ->
                                    mdl |> withNoCmd

                                Ok url ->
                                    { mdl
                                        | meme =
                                            { meme
                                                | image =
                                                    { url = url
                                                    , hash = hash
                                                    }
                                            }
                                    }
                                        |> withCmd
                                            (if not mdl.showMemeImage then
                                                Cmd.none

                                             else
                                                get persistenceKeys.shownimageurl mdl
                                            )

        ReceiveModel value ->
            case value of
                Nothing ->
                    model |> withNoCmd

                Just v ->
                    case ED.decodeSavedModel v of
                        Err _ ->
                            model |> withNoCmd

                        Ok savedModel ->
                            { model | receivedModel = Just savedModel }
                                |> withNoCmd

        ReceiveMemeKeys keys ->
            receiveMemeKeys keys model

        ReceiveImageUrls keys ->
            let
                urls =
                    List.map decodeSubkey keys
                        |> List.map Tuple.second
            in
            { model
                | knownImages =
                    Set.fromList <| Debug.log "ReveiveImageUrls" urls
            }
                |> loadThumbnails

        ReceiveThumbnail hash value ->
            case value of
                Just v ->
                    case JD.decodeValue JD.string v of
                        Err _ ->
                            loadNextThumbnail model

                        Ok url ->
                            let
                                image =
                                    { url = url, hash = hash }
                            in
                            { model
                                | thumbnails =
                                    Dict.insert hash image model.thumbnails
                            }
                                |> loadNextThumbnail

                Nothing ->
                    -- No thumbnail exists, need to create one
                    -- Start by loading the image URL.
                    -- Will continue at ReceiveImageForThumbnail
                    ( model
                    , getLabeled imageForThumbnailLabel
                        (encodeSubkey persistenceKeys.imageurls hash)
                        model
                    )

        ReceiveImageForThumbnail hash value ->
            case value of
                Nothing ->
                    loadNextThumbnail model

                Just v ->
                    case JD.decodeValue JD.string v of
                        Err _ ->
                            loadNextThumbnail model

                        Ok url ->
                            -- Got the image. Need to compute its size
                            -- Will continue at ReceiveThumbnailProperties
                            { model
                                | thumbnailImageUrl = url
                                , triggerThumbnailProperties =
                                    model.triggerThumbnailProperties + 1
                            }
                                |> withNoCmd

        ReceiveThumbnailProperties properties ->
            let
                props =
                    Debug.log "ReceiveThumbnailProperties" properties
            in
            -- TODO
            { model | thumbnailImageUrl = "" }
                |> loadNextThumbnail

        GetReturnedFile ->
            { model
                | triggerReturnedFile = model.triggerReturnedFile + 1
                , savedSelectedPosition =
                    if model.showMemeImage then
                        model.savedSelectedPosition

                    else
                        model.selectedPosition
                , selectedPosition = Nothing
            }
                |> withNoCmd

        ReceiveReturnedFile returnedFile ->
            let
                downloadFile =
                    returnedFile.file
            in
            { model | downloadFile = Just downloadFile }
                |> withCmd
                    (if Debug.log "canDownload" returnedFile.canDownload then
                        Task.perform ReceiveReturnedBytes <|
                            File.toBytes downloadFile

                     else
                        Task.perform ReceiveMemeImageUrl <|
                            File.toUrl downloadFile
                    )

        ReceiveReturnedBytes bytes ->
            case model.downloadFile of
                Nothing ->
                    model |> withNoCmd

                Just file ->
                    { model
                        | downloadFile = Nothing
                        , selectedPosition = model.savedSelectedPosition
                        , savedSelectedPosition = Nothing
                    }
                        |> withCmd
                            (Download.bytes (File.name file)
                                (File.mime file)
                                bytes
                            )

        ReceiveMemeImageUrl url ->
            { model
                | downloadFile = Nothing
                , showMemeImage = Debug.log "ReceiveMemeImageUrl" True
                , memeImageUrl = Just url
            }
                |> withCmd
                    (put persistenceKeys.shownimageurl
                        (Just <| JE.string url)
                        model
                    )

        ShowMemeImage show mimeType ->
            if show then
                { model
                    | triggerReturnedUrl = model.triggerReturnedUrl + 1
                    , mimeType = mimeType
                    , savedSelectedPosition = model.selectedPosition
                    , selectedPosition = Nothing
                }
                    |> withNoCmd

            else
                { model
                    | showMemeImage = False
                    , savedSelectedPosition = Nothing
                    , selectedPosition = model.savedSelectedPosition
                }
                    |> withCmd
                        (put persistenceKeys.shownimageurl Nothing model)

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
                            { image
                                | url = inputs.imageUrl
                                , hash = MD5.hex inputs.imageUrl
                            }
                    }
                , showMemeImage = False
                , savedSelectedPosition = Nothing
                , selectedPosition =
                    if model.showMemeImage then
                        model.savedSelectedPosition

                    else
                        model.selectedPosition
                , triggerImageProperties = model.triggerImageProperties + 1
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
            case SB.checkSubscription m button of
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
            updateInternal (SetAlignment <| stringToAlignment string) model

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


loadThumbnails : Model -> ( Model, Cmd Msg )
loadThumbnails model =
    let
        neededThumbnails =
            Debug.log "neededThumbnails"
                (Dict.keys model.thumbnails
                    |> Set.fromList
                    |> Set.diff model.knownImages
                    |> Set.toList
                )
    in
    case neededThumbnails of
        [] ->
            model |> withNoCmd

        hash :: rest ->
            { model | neededThumbnails = rest }
                |> withCmd (getThumbnail hash model)


loadNextThumbnail : Model -> ( Model, Cmd Msg )
loadNextThumbnail model =
    case model.neededThumbnails of
        [] ->
            model |> withNoCmd

        hash :: tail ->
            { model | neededThumbnails = tail }
                |> withCmd (getThumbnail hash model)


receiveMemeKeys : List String -> Model -> ( Model, Cmd Msg )
receiveMemeKeys keys model =
    let
        savedMemes =
            List.map decodeSubkey keys
                |> List.map Tuple.second
                |> List.foldr Set.insert model.savedMemes
    in
    { model | savedMemes = savedMemes }
        |> withNoCmd


useInitialMeme : Model -> ( Model, Cmd Msg )
useInitialMeme model =
    ( { model | meme = initialMeme }
      -- To get the size
    , Task.perform ReceiveImageUrl <|
        Task.succeed initialImage.url
    )


receivePutImageImages : String -> String -> Maybe Value -> Cmd Msg
receivePutImageImages url hash value =
    if value == Nothing then
        Task.perform (PutImageUrl hash) <| Task.succeed url

    else
        Task.perform ImageExists <| Task.succeed hash


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
                | image =
                    { image
                        | url = url
                        , hash = MD5.hex url
                    }
            }
        , showMemeImage = False
        , savedSelectedPosition = Nothing
        , selectedPosition =
            if model.showMemeImage then
                model.savedSelectedPosition

            else
                model.selectedPosition
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
    case Debug.log "storageHandler" response of
        LocalStorage.GetResponse { label, key, value } ->
            let
                ( pkey, subkey ) =
                    decodeSubkey key
            in
            if pkey == persistenceKeys.images then
                -- images exists flag, indexed as "images.<hash>"
                ( model
                , Task.perform (ReceiveImageKey subkey) (Task.succeed value)
                )

            else if pkey == persistenceKeys.meme then
                -- current Meme, indexed as "meme>"
                ( model
                , Task.perform ReceiveMeme (Task.succeed value)
                )

            else if pkey == persistenceKeys.model then
                -- SavedModel, indexed as "model"
                ( model
                , Task.perform ReceiveModel (Task.succeed value)
                )

            else if pkey == persistenceKeys.imageurls then
                -- "data:..." URL for image, indexes as "imageurls.<hash>"
                ( model
                , if Just imageForThumbnailLabel == label then
                    Task.perform (ReceiveImageForThumbnail subkey)
                        (Task.succeed value)

                  else
                    Task.perform (ReceiveImage subkey) (Task.succeed value)
                )

            else if pkey == persistenceKeys.memes then
                -- Meme, indexed as "memes.<saved name>"
                ( { model | dialog = NoDialog }
                , Task.perform ReceiveMeme (Task.succeed value)
                )

            else if pkey == persistenceKeys.thumbnails then
                -- "data:..." URL, indexed as "thumbnails.<hash>"
                ( model
                , Task.perform (ReceiveThumbnail subkey) (Task.succeed value)
                )

            else
                model |> withNoCmd

        LocalStorage.ListKeysResponse { label, keys } ->
            ( model
            , Task.succeed keys
                |> Task.perform
                    (if label == Just listImageUrlsLabel then
                        ReceiveImageUrls

                     else
                        ReceiveMemeKeys
                    )
            )

        _ ->
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


maxMemeSize : Model -> ( Int, Int )
maxMemeSize model =
    let
        ( w, h ) =
            model.windowSize
    in
    ( w * 95 // 100, h // 2 )


view : Model -> Document Msg
view model =
    let
        meme =
            model.meme

        scale =
            scalewh ( model.maxWidth, model.maxHeight ) ( meme.width, meme.height )

        scalememe =
            scalewh (maxMemeSize model) ( scale.width, scale.height )
    in
    { title = "ZAP Meme"
    , body =
        [ div
            [ align "center"
            ]
            [ span []
                [ if model.showMemeImage && model.memeImageUrl /= Nothing then
                    case model.memeImageUrl of
                        Nothing ->
                            text ""

                        Just url ->
                            img
                                [ src url
                                , width <| tos scalememe.width ++ "px"
                                , height <| tos scalememe.height ++ "px"
                                , alt "Meme image"
                                , onClick <| ShowMemeImage False ""
                                ]
                                []

                  else
                    renderMeme
                        { scalememe
                            | scale = scale.scale * scalememe.scale
                        }
                        model
                , case model.thumbnailImageUrl of
                    "" ->
                        text ""

                    url ->
                        svg
                            [ Svg.Attributes.id thumbnailSvgId
                            , width thumbnailImageWidth
                            , height thumbnailImageHeight
                            ]
                            [ Svg.Lazy.lazy renderThumbnailImage url ]
                , ImageProperties.imageProperties
                    [ ImageProperties.imageId thumbnailImageId
                    , ImageProperties.triggerImageProperties
                        model.triggerThumbnailProperties
                    , ImageProperties.onImageProperties ReceiveThumbnailProperties
                    ]
                    []
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
                    , SvgToDataUrl.returnedUrlParameters svgId model.mimeType
                    , SvgToDataUrl.triggerReturnedUrl model.triggerReturnedUrl
                    , SvgToDataUrl.onReturnedUrl
                        (\returnedUrl ->
                            ReceiveMemeImageUrl returnedUrl.url
                        )
                    ]
                    []
                ]
            , p [] <|
                List.concat
                    [ if model.showMemeImage then
                        [ button [ onClick <| ShowMemeImage False "" ]
                            [ text "Edit Meme (image shown above)" ]
                        ]

                      else
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
                        , button [ onClick <| ShowMemeImage True "image/jpeg" ]
                            [ text "Show Image" ]
                        ]
                    , [ br
                      , renderInputs scale scalememe model
                      ]
                    ]
            , button [ onClick ToggleHelp ]
                [ text <|
                    if model.showHelp then
                        "Hide Help"

                    else
                        "Show Help"
                ]
            , button [ onClick <| SetDialog MemesDialog ]
                [ text "Memes" ]
            , button [ onClick <| SetImagesDialog ]
                [ text "Images" ]
            , if model.showHelp then
                helpParagraph

              else
                text ""
            , fontParagraph
            , p []
                [ h2 [ style "margin-bottom" "0" ] [ text "ZAP Meme" ]
                , text <| chars.copyright ++ " 2019 Bill St. Clair"
                , br
                , a
                    [ href "https://github.com/billstclair/zapmeme"
                    , target "_blank"
                    ]
                    [ text "GitHub" ]
                ]
            , Dialog.render (dialogConfig model) (model.dialog /= NoDialog)
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


thead : String -> Html msg
thead string =
    Html.th
        [ textalign "center"
        , style "font-weight" "bold"
        ]
        [ text string ]


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


nbspd : List (Html Msg) -> Html Msg
nbspd elements =
    span [ style "white-space" "nowrap" ] <|
        List.intersperse (text chars.nbsp) elements


renderInputs : ScaleWH -> ScaleWH -> Model -> Html Msg
renderInputs scale scalememe model =
    let
        inputs =
            model.inputs

        isDisabled =
            model.selectedPosition == Nothing

        meme =
            model.meme

        isOutlined =
            inputs.isOutlined

        outlineColor =
            if isOutlined then
                inputs.outlineColor

            else
                "none"
    in
    span []
        [ table []
            [ tr []
                [ td []
                    [ textarea
                        [ onInput SetText
                        , rows 4
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
        , p [ style "margin-bottom" "0.5em" ]
            [ b "Selected Caption" ]
        , p [ style "margin-top" "0" ]
            [ nbspd
                [ controllerRadio isDisabled CaptionWidthController model
                , text "Width: "
                , span []
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
            , text " "
            , nbspd
                [ controllerRadio isDisabled CaptionHeightController model
                , b "Height:"
                , span []
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
            , text " "
            , nbspd
                [ b "Position:"
                , positionSelector isDisabled model
                ]
            , text " "
            , nbspd
                [ b "Borders:"
                , input
                    [ type_ "checkbox"
                    , onCheck SetShowCaptionBorders
                    , checked model.showCaptionBorders
                    ]
                    []
                ]
            , text " "
            , nbspd
                [ b "Alignment:"
                , alignmentSelector isDisabled model
                ]
            , text " "
            , nbspd
                [ b "Font: "
                , fontSelector isDisabled model
                ]
            , text " "
            , nbspd
                [ controllerRadio isDisabled FontHeightController model
                , b "Height:"
                , span []
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
            , text " "
            , nbspd
                [ b "Bold: "
                , input
                    [ type_ "checkbox"
                    , disabled isDisabled
                    , onCheck SetBold
                    , checked inputs.bold
                    ]
                    []
                ]
            , text " "
            , nbspd
                [ b "Color: "
                , colorSelector False False isDisabled inputs.fontcolor
                , input
                    [ type_ "text"
                    , disabled isDisabled
                    , size 10
                    , onInput SetFontColor
                    , value inputs.fontcolor
                    ]
                    []
                ]
            , text " "
            , nbspd
                [ b "Outline: "
                , colorSelector True (not isOutlined) isDisabled inputs.outlineColor
                , input
                    [ type_ "text"
                    , disabled (isDisabled || not isOutlined)
                    , size 10
                    , onInput SetOutlineColor
                    , value outlineColor
                    ]
                    []
                ]
            ]
        , p [ style "margin-bottom" "0.5em" ]
            [ b "Background" ]
        , p [ style "margin-top" "0" ]
            [ nbspd
                [ b "Image:"
                , button [ onClick <| ReceiveImageUrl initialImage.url ]
                    [ text "Default" ]
                , button [ onClick SelectImageFile ]
                    [ text "Choose" ]
                ]
            , text " "
            , nbspd
                [ input
                    [ type_ "text"
                    , size 15
                    , onInput SetImageUrl
                    , value inputs.imageUrl
                    ]
                    []
                , button [ onClick SetMemeImageUrl ]
                    [ text "URL" ]
                ]
            , text " "
            , nbspd
                [ controllerRadio False MaxWidthController model
                , b "Max Width:"
                , input
                    [ type_ "text"
                    , size 5
                    , onInput SetMaxWidth
                    , value <| tos model.maxWidth
                    ]
                    []
                ]
            , text " "
            , nbspd
                [ controllerRadio False MaxHeightController model
                , b "Height:"
                , input
                    [ type_ "text"
                    , size 5
                    , onInput SetMaxHeight
                    , value <| tos model.maxHeight
                    ]
                    []
                ]
            , nbspd
                [ span [ style "font-size" "80%" ]
                    [ text " ("
                    , text <| tos meme.width
                    , text "x"
                    , text <| tos meme.height
                    , text ") Scaled: ("
                    , text <| tos scale.width
                    , text "x"
                    , text <| tos scale.height
                    , text ")"
                    ]
                ]
            , if scalememe.scale >= 1 then
                text ""

              else
                nbspd
                    [ span [ style "font-size" "80%" ]
                        [ text " Displayed: ("
                        , text <| tos scalememe.width
                        , text "x"
                        , text <| tos scalememe.height
                        , text ")"
                        ]
                    ]
            , br
            , nbspd
                [ b "File Name: "
                , input
                    [ type_ "text"
                    , size 15
                    , onInput SetFileName
                    , value inputs.fileName
                    ]
                    []
                ]
            , text " "
            , nbspd
                [ b "Download:"
                , button [ onClick <| StartDownload "image/jpeg" ".jpg" ]
                    [ text "JPEG" ]
                , button [ onClick <| StartDownload "image/png" ".png" ]
                    [ text "PNG" ]
                ]
            , if String.startsWith "data:" model.meme.image.url then
                text ""

              else
                p []
                    [ text "Background images specified as URLs will NOT appear in saved images. Upload the image from your computer to prevent this." ]
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


helpParagraph : Html Msg
helpParagraph =
    div [ class "help" ]
        [ Markdown.toHtml
            [ style "text-align" "left"
            , style "margin" "auto"
            ]
            helpText
        , button [ onClick ToggleHelp ]
            [ text "Hide Help" ]
        ]


helpText : String
helpText =
    """
You will usually want to choose an image other than the default from
the "Background" section. "Max Width" and its "Height" are the maximum
background image size, in pixels. The image will be scaled to fit. The
"JPEG" and "PNG" download buttons will save to your computer an image
of the completed meme in "File Name", with suitable extension (if your
browser supports it. On mobile, they will change the image at the top
of the screen to be an image you can long-tap to save or copy).

If you specify a "URL" for the image, it will NOT appear in saved
versions. you'll have to do a screen capture to get the meme. This is
a browser security issue. The fix is to "Choose" the background image
from your computer.

To add a new caption, click "Add Caption". This will use the text in
the text input area, just below the "Add Caption" button, a new
"Position", not occupied by any existing captions, and the other
settings below "Selected Caption".

To change the image to a JPEG image file you can save or copy, click
"Show Image". To change it back, click "Edit Meme".

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
any browser. You may also click on a font in the the "Fonts" list at
the bottom of the page. I plan to extend this, enabling fonts from
Google and others, and allowing you to show a subset of the available
fonts, if you don't expect to use some of them.

"Height" is the height of the font, in percentage of the image
height. The arrow buttons change this by 1, but you can type a single
decimal digit, if you need fine tuning (e.g. "7.5").

"Font Color" provides a list of choices, but you can type any HTML
font in the text area to its right. E.g. "#8f0" for a bright green (a
little red mixed in).

If you choose an "Outline Color", the text will stand out better from
the background image. Again, you may enter a custom color. I find
"impact" and "arial-black" to be the best-looking outlined fonts.

Click "Hide Help" to hide this text, and "Show Help" to show it again.

Click "Memes" to bring up a dialog where you can save the current meme, or load or delete previously saved memes. The "Save Meme" button there will save the current meme with the name in the text box to its left. Clicking an "O" button in the "Load" column will load that meme. Clicking an "X" button in the "Delete" column will delete that meme. The images will remain, so you'll only lose the meme text and sizing information.
         """


fontParagraph : Html Msg
fontParagraph =
    p [ style "width" "80%" ] <|
        List.concat
            [ [ span
                    [ style "font-size" "110%"
                    , style "font-weight" "bold"

                    --, style "font-weight" "bold"
                    ]
                    [ text "Fonts (click to select)" ]
              , br
              ]
            , List.map fontExample safeFontList
            ]


fontExample : Font -> Html Msg
fontExample font =
    span []
        [ span
            [ fontAttribute font
            , onClick <| SetFont font.font
            ]
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


thumbnailImageWidth : String
thumbnailImageWidth =
    "200"


thumbnailImageHeight : String
thumbnailImageHeight =
    "150"


renderThumbnailImage : String -> Svg Msg
renderThumbnailImage url =
    Svg.image
        [ Svg.Attributes.id thumbnailImageId
        , width thumbnailImageWidth
        , height thumbnailImageHeight
        , xlinkHref url
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
                    , strokeWidth "2"
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


thumbnailSvgId : String
thumbnailSvgId =
    "thumbnail-svg"


thumbnailImageId : String
thumbnailImageId =
    "thumbnail-image"


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


car : List String -> String
car list =
    Maybe.withDefault "" <| List.head list


cdr : List a -> List a
cdr list =
    Maybe.withDefault [] <| List.tail list



{- Dialogs -}


{-| Not currently used. Shown inline.
-}
helpDialog : Model -> Config Msg
helpDialog model =
    { styles = []
    , title = "Help"
    , content =
        let
            ( _, h ) =
                model.windowSize
        in
        [ div
            [ style "max-height" (tos (h * 3 // 10) ++ "px")
            , style "text-align" "left"
            ]
            [ Markdown.toHtml [] helpText ]
        ]
    , actionBar = [ dismissDialogButton ]
    }


memesDialog : Model -> Config Msg
memesDialog model =
    let
        inputs =
            model.inputs

        wouldOverwrite =
            Set.member inputs.savedMemeName model.savedMemes
    in
    { styles = []
    , title = "Memes"
    , content =
        [ input
            [ type_ "text"
            , autofocus True
            , size 40
            , onInput SetSavedMemeName
            , value inputs.savedMemeName
            ]
            []
        , text " "
        , button
            [ onClick SaveMeme
            , disabled <| inputs.savedMemeName == ""
            ]
            [ text "Save Meme" ]
        , if wouldOverwrite then
            span []
                [ br
                , text "Save will overwrite an existing saved meme."
                ]

          else
            text ""
        , br
        , br
        , table
            []
            (tr []
                [ thead "Name"
                , thead "Load"
                , thead "Delete"
                ]
                :: List.map savedMemeRow (Set.toList model.savedMemes)
            )
        ]
    , actionBar = [ dismissDialogButton ]
    }


savedMemeRow : String -> Html Msg
savedMemeRow name =
    tr []
        [ td [] [ text name ]
        , td [ align "center" ]
            [ button
                [ onClick <| LoadSavedMeme name ]
                [ text "O" ]
            ]
        , td [ align "center" ]
            [ button
                [ onClick <| DeleteSavedMeme name ]
                [ text "X" ]
            ]
        ]


{-| This doesn't work yet.

We really need saved thumbnails to avoid displaying and reducing 100
large data urls.

-}
imagesDialog : Model -> Config Msg
imagesDialog model =
    let
        inputs =
            model.inputs

        images =
            model.images

        imageList =
            Dict.toList images
                |> List.map Tuple.second
    in
    { styles = []
    , title = "Images"
    , content =
        [ table
            []
            (tr []
                [ thead "Image"
                , thead "Delete"
                , thead "Memes"
                ]
                :: List.map (savedImageRow model) imageList
            )
        ]
    , actionBar = [ dismissDialogButton ]
    }


savedImageRow : Model -> Image -> Html Msg
savedImageRow name image =
    tr []
        [ td []
            [ img
                [ src image.url
                , alt image.hash
                , width "100px"
                ]
                []
            ]
        , td [ align "center" ]
            [ button
                [ onClick <| DeleteSavedImage image.hash ]
                [ text "X" ]
            ]
        , td [] [ text "TODO" ]
        ]


dismissDialogButton : Html Msg
dismissDialogButton =
    button [ onClick <| SetDialog NoDialog ]
        [ text "Close" ]


noDialog : Model -> Config Msg
noDialog model =
    let
        dialog =
            ED.encodeWhichDialog model.dialog
                |> JE.encode 0
    in
    { styles = []
    , title = "Missing Dialog Config"
    , content =
        [ text <| "You need to define a dialog config for " ++ dialog ]
    , actionBar =
        [ dismissDialogButton ]
    }


dialogConfig : Model -> Config Msg
dialogConfig model =
    case model.dialog of
        HelpDialog ->
            helpDialog model

        MemesDialog ->
            memesDialog model

        ImagesDialog ->
            imagesDialog model

        _ ->
            noDialog model



{-

      Persistence

   Keys are in `persistenceKeys`.

   model: An instance of `SavedModel`.
   meme: The Current meme. An instance of `Meme`.
   memes: Saved memes. "memes.<name>" is an instance of `Meme`.
   images: Saved images. "images.<hash>" is the string with the MD5 <hash>.

   Images are serialized to the hash of their url, so we get only one
   copy of each.

-}


localStoragePrefix : String
localStoragePrefix =
    "zapmeme"


encodeSubkey : String -> String -> String
encodeSubkey key subkey =
    key ++ "." ++ subkey


decodeSubkey : String -> ( String, String )
decodeSubkey fullkey =
    String.split "." fullkey
        |> (\list ->
                ( car list, String.join "." <| cdr list )
           )


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { dialog = model.dialog
    , selectedPosition = model.selectedPosition
    , savedSelectedPosition = model.savedSelectedPosition
    , showCaptionBorders = model.showCaptionBorders
    , maxWidth = model.maxWidth
    , maxHeight = model.maxHeight
    , inputs = model.inputs
    , showMemeImage = model.showMemeImage
    , showHelp = model.showHelp
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    { model
        | dialog = savedModel.dialog
        , selectedPosition = savedModel.selectedPosition
        , savedSelectedPosition =
            savedModel.savedSelectedPosition
        , showCaptionBorders = savedModel.showCaptionBorders
        , maxWidth = savedModel.maxWidth
        , maxHeight = savedModel.maxHeight
        , inputs = savedModel.inputs
        , showMemeImage = savedModel.showMemeImage
        , showHelp = savedModel.showHelp
    }


get : String -> Model -> Cmd Msg
get key model =
    localStorageSend (LocalStorage.get <| Debug.log "get" key) model


getLabeled : String -> String -> Model -> Cmd Msg
getLabeled label key model =
    localStorageSend
        (LocalStorage.getLabeled label <|
            Debug.log ("getLabeled " ++ label) key
        )
        model


put : String -> Maybe Value -> Model -> Cmd Msg
put key value model =
    let
        k =
            Debug.log "put" key
    in
    localStorageSend (LocalStorage.put key value) model


listKeys : String -> Model -> Cmd Msg
listKeys prefix model =
    localStorageSend (LocalStorage.listKeys <| Debug.log "listKeys" prefix) model


listKeysLabeled : String -> String -> Model -> Cmd Msg
listKeysLabeled label prefix model =
    localStorageSend
        (LocalStorage.listKeysLabeled label <|
            Debug.log ("listKeysLabeled " ++ label) prefix
        )
        model


putModel : Model -> Cmd Msg
putModel model =
    let
        savedModel =
            modelToSavedModel model

        value =
            ED.encodeSavedModel savedModel
    in
    Cmd.batch
        [ put persistenceKeys.model (Just value) model
        , putMeme model.meme model
        ]


putMeme : Meme -> Model -> Cmd Msg
putMeme meme model =
    let
        image =
            meme.image

        url =
            image.url

        urlHash =
            image.hash

        value =
            ED.encodeMeme meme
    in
    Cmd.batch
        [ put persistenceKeys.meme (Just value) model
        , Task.perform (MaybePutImageUrl urlHash) <| Task.succeed url
        ]


putSavedMeme : String -> Maybe Meme -> Model -> Cmd Msg
putSavedMeme name meme model =
    let
        key =
            encodeSubkey persistenceKeys.memes name

        value =
            case meme of
                Nothing ->
                    Nothing

                Just m ->
                    Just <| ED.encodeMeme m
    in
    put key value model


getSavedMeme : String -> Model -> Cmd Msg
getSavedMeme name model =
    let
        key =
            encodeSubkey persistenceKeys.memes name
    in
    get key model


listImages : Model -> Cmd Msg
listImages model =
    listKeysLabeled listImageUrlsLabel
        (persistenceKeys.imageurls ++ ".")
        model


getThumbnail : String -> Model -> Cmd Msg
getThumbnail hash model =
    let
        key =
            encodeSubkey persistenceKeys.thumbnails hash
    in
    get key model


imageForThumbnailLabel : String
imageForThumbnailLabel =
    "imageForThumbnail"


listImageUrlsLabel : String
listImageUrlsLabel =
    "listImageUrls"


{-| Plural means there are subkeys, e.g. "memes.<name>","images.<hash>"
-}
persistenceKeys =
    { model = "model"
    , meme = "meme"
    , memes = "memes"
    , images = "images"
    , imageurls = "imageurls"
    , thumbnails = "thumbnails"
    , shownimageurl = "shownimageurl"
    }
