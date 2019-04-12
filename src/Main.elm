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
import PortFunnel.LocalStorage.Sequence as Sequence
    exposing
        ( DbRequest(..)
        , DbResponse(..)
        , KeyPair
        )
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
        , StorageMirror
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
    , localStorageStates : LocalStorageStates
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
    , thumbnails : Dict String Image
    , thumbnailImageUrl : String
    , thumbnailImageHash : String
    , thumbnailImageSize : ( Int, Int )
    , triggerThumbnailProperties : Int
    , triggerThumbnailUrl : Int
    , neededThumbnails : List String
    , savedMemes : Set String
    , neededMemes : List String
    , memeImages : Dict String String -- meme name -> image hash
    , imageMemes : Dict String (List String) -- image hash -> list of meme names
    , loadedImages : List String
    , storageText : String
    , loadedStorage : StorageMirror
    , expectedStorageKeys : { memes : Int, images : Int }
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
    , showAllImages = True
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
    | OnKeyPress String
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
    | CheckLoadedImage String Bool
    | LoadCheckedImages
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
    | SetShowAllImages Bool
    | SetStorageText String
    | LoadStorageMirror
    | ReceiveStorageMemeKeys (List String)
    | ReceiveStorageImageKeys (List String)
    | ReceiveStorageMeme String (Maybe Value)
    | ReceiveStorageImage String (Maybe Value)
    | CheckStorageDone
    | StoreStorageMirror
    | WaitForStore Int Posix
    | CloseDataDialog
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
    | ReceiveMemeImageHash String (Maybe Value)
    | ReceiveThumbnail String (Maybe Value)
    | ReceiveImageForThumbnail String (Maybe Value)
    | ReceiveThumbnailProperties ImageProperties
    | GetThumbnailUrl ()
    | ReceiveThumbnailUrl String
    | GetImageFromDialog String
    | ReceiveImageFromDialog String (Maybe Value)
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | SequenceDone (Model -> ( Model, Cmd Msg ))
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


keyPressDecoder : Decoder Msg
keyPressDecoder =
    JD.field "key" JD.string
        |> JD.andThen
            (OnKeyPress >> JD.succeed)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every shortRepeatTimeDelay Tick
        , Events.onResize WindowResize
        , Events.onKeyDown keyPressDecoder
        , PortFunnels.subscriptions ProcessLocalStorage model
        ]


buttonSize : Float
buttonSize =
    40


buttonPair : ( Float, Float )
buttonPair =
    ( buttonSize, buttonSize )


funnelState : PortFunnels.State
funnelState =
    PortFunnels.initialState localStoragePrefix


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
            , localStorageStates = initialStorageStates
            , windowSize = ( 2000, 2000 )
            , subscription = Nothing
            , fontDict = safeFontDict
            , key = key
            , receiveImagesHandler = Nothing
            , receivedMeme = Nothing
            , receivedModel = Nothing
            , knownImages = Set.empty
            , thumbnails = Dict.empty
            , thumbnailImageUrl = ""
            , thumbnailImageHash = ""
            , thumbnailImageSize = ( 0, 0 )
            , triggerThumbnailProperties = 0
            , triggerThumbnailUrl = 0
            , neededThumbnails = []
            , savedMemes = Set.empty
            , neededMemes = []
            , memeImages = Dict.empty
            , imageMemes = Dict.empty
            , loadedImages = []
            , storageText = ""
            , loadedStorage = StorageMirror [] []
            , expectedStorageKeys = { memes = -1, images = -1 }
            , msg = Nothing
            }
    in
    model
        |> withCmds
            [ Task.perform getViewport Dom.getViewport
            , get pK.meme
            , get pK.model
            , listKeys <| pK.memes ++ "."
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
                        , showAllImages = model.inputs.showAllImages
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

        OnKeyPress key ->
            ( if key == "Escape" then
                { model | dialog = NoDialog }

              else
                model
            , Cmd.none
            )

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
            setImagesDialog model

        SetSavedMemeName name ->
            { model | inputs = { inputs | savedMemeName = name } }
                |> withNoCmd

        SaveMeme ->
            { model
                | dialog = NoDialog
                , savedMemes =
                    Set.insert inputs.savedMemeName model.savedMemes
            }
                |> withCmd (putSavedMeme inputs.savedMemeName (Just model.meme))

        LoadSavedMeme name ->
            { model
                | savedMemes = Set.insert name model.savedMemes
                , showMemeImage = False
                , inputs = { inputs | savedMemeName = name }
            }
                |> withCmd (getSavedMeme name)

        DeleteSavedMeme name ->
            let
                maybeHash =
                    Dict.get name model.memeImages
            in
            { model
                | savedMemes = Set.remove name model.savedMemes
                , memeImages =
                    Dict.remove name model.memeImages
                , imageMemes =
                    case maybeHash of
                        Nothing ->
                            model.imageMemes

                        Just hash ->
                            Dict.insert hash
                                (Dict.get hash model.imageMemes
                                    |> Maybe.withDefault []
                                    |> List.filter ((/=) name)
                                )
                                model.imageMemes
            }
                |> withCmd (putSavedMeme name Nothing)

        DeleteSavedImage hash ->
            let
                memes =
                    Dict.get hash model.imageMemes |> Maybe.withDefault []
            in
            { model
                | knownImages = Set.remove hash model.knownImages
                , thumbnails = Dict.remove hash model.thumbnails
                , imageMemes = Dict.remove hash model.imageMemes
                , memeImages =
                    List.foldr Dict.remove model.memeImages memes
                , savedMemes =
                    List.foldr Set.remove model.savedMemes memes
                , meme =
                    if hash == model.meme.image.hash then
                        initialMeme

                    else
                        model.meme
            }
                |> withCmds
                    (List.concat
                        [ [ clear (encodeSubkey pK.images hash)
                          , clear (encodeSubkey pK.imageurls hash)
                          , clear (encodeSubkey pK.thumbnails hash)
                          ]
                        , List.map
                            (\name ->
                                clear (encodeSubkey pK.memes name)
                            )
                            memes
                        ]
                    )

        CheckLoadedImage hash checked ->
            { model
                | loadedImages =
                    if checked then
                        adjoin hash model.loadedImages

                    else
                        List.filter ((/=) hash) model.loadedImages
            }
                |> withNoCmd

        LoadCheckedImages ->
            let
                images =
                    model.loadedImages

                imageMemes =
                    model.imageMemes

                memes =
                    List.foldl
                        (\hash res ->
                            List.concat
                                [ Dict.get hash imageMemes
                                    |> Maybe.withDefault []
                                , res
                                ]
                        )
                        []
                        images
            in
            ( { model
                | dialog = DataDialog
                , storageText = "Loading..."
                , expectedStorageKeys =
                    Debug.log "LoadCheckedImages"
                        { memes = List.length memes
                        , images = List.length images
                        }
              }
            , [ memes
                    |> List.map
                        (\name ->
                            let
                                key =
                                    encodeSubkey pK.memes name
                            in
                            getLabeled labels.storageMeme key
                        )
              , images
                    |> List.map
                        (\hash ->
                            let
                                key =
                                    encodeSubkey pK.imageurls hash
                            in
                            getLabeled labels.storageImage key
                        )
              ]
                |> List.concat
                |> Cmd.batch
            )

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
                        encodeSubkey pK.images hash
                in
                { model | receiveImagesHandler = Just <| receivePutImageImages url }
                    |> withCmd (get flagKey)

        ImageExists hash ->
            { model
                | knownImages = Set.insert hash model.knownImages
            }
                |> withNoCmd

        PutImageUrl hash url ->
            let
                flagKey =
                    encodeSubkey pK.images hash

                key =
                    encodeSubkey pK.imageurls hash
            in
            -- I don't know where this comes from yet, but we don't want to save it.
            if hash == "" then
                model |> withNoCmd

            else
                { model
                    | knownImages = Set.insert hash model.knownImages
                }
                    |> withCmds
                        [ put flagKey (Just <| JE.bool True)
                        , put key (Just <| JE.string url)
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
            let
                mdl =
                    { model | dialog = NoDialog }
            in
            case value of
                Nothing ->
                    useInitialMeme mdl

                Just v ->
                    case ED.decodeMeme v of
                        Err _ ->
                            useInitialMeme mdl

                        Ok meme ->
                            ( { mdl | receivedMeme = Just meme }
                            , get
                                (encodeSubkey pK.imageurls
                                    meme.image.hash
                                )
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
            case value of
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
                                                get pK.shownimageurl
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
                                |> (if savedModel.dialog == ImagesDialog then
                                        setImagesDialog

                                    else
                                        withNoCmd
                                   )

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
                    Set.fromList urls
            }
                |> loadThumbnails

        ReceiveMemeImageHash name value ->
            case value of
                Nothing ->
                    loadNextMemeImage model

                Just v ->
                    case ED.decodeMeme v of
                        Err _ ->
                            loadNextMemeImage model

                        Ok meme ->
                            let
                                hash =
                                    meme.image.hash

                                names =
                                    Dict.get hash model.imageMemes
                                        |> Maybe.withDefault []
                            in
                            { model
                                | memeImages =
                                    Dict.insert name hash model.memeImages
                                , imageMemes =
                                    Dict.insert hash
                                        (adjoin name names)
                                        model.imageMemes
                            }
                                |> loadNextMemeImage

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
                    , getLabeled labels.imageForThumbnail
                        (encodeSubkey pK.imageurls hash)
                    )

        ReceiveImageForThumbnail hash value ->
            case value of
                Nothing ->
                    loadNextThumbnail model

                Just v ->
                    case JD.decodeValue JD.string v of
                        Err _ ->
                            loadNextThumbnail model

                        -- I don't know how this happens, but it does.
                        Ok "" ->
                            loadNextThumbnail model

                        Ok url ->
                            -- Got the image. Need to compute its size
                            -- Will continue at ReceiveThumbnailProperties
                            { model
                                | thumbnailImageUrl = url
                                , thumbnailImageHash = hash
                                , thumbnailImageSize =
                                    ( thumbnailImageWidth, thumbnailImageHeight )
                                , triggerThumbnailProperties =
                                    model.triggerThumbnailProperties + 1
                            }
                                |> withNoCmd

        ReceiveThumbnailProperties properties ->
            let
                w =
                    properties.width * thumbnailScaledHeight // properties.height
            in
            { model
                | thumbnailImageSize = ( w, thumbnailScaledHeight )
            }
                -- Need to delay to let the Svg draw
                |> withCmd (Task.perform GetThumbnailUrl <| Task.succeed ())

        GetThumbnailUrl _ ->
            { model
              -- The action continues at ReceiveThumbnailUrl
                | triggerThumbnailUrl = model.triggerThumbnailUrl + 1
            }
                |> withNoCmd

        ReceiveThumbnailUrl url ->
            let
                hash =
                    model.thumbnailImageHash

                image =
                    { url = url, hash = hash }

                ( mdl, cmd ) =
                    { model | thumbnails = Dict.insert hash image model.thumbnails }
                        |> loadNextThumbnail
            in
            mdl |> withCmds [ cmd, putThumbnail hash url ]

        GetImageFromDialog hash ->
            { model | showMemeImage = False }
                |> withCmd
                    (getLabeled labels.imageFromDialog
                        (encodeSubkey pK.imageurls hash)
                    )

        ReceiveImageFromDialog hash value ->
            case value of
                Nothing ->
                    model |> withNoCmd

                Just v ->
                    case JD.decodeValue JD.string v of
                        Err _ ->
                            model |> withNoCmd

                        Ok url ->
                            let
                                meme =
                                    model.meme

                                image =
                                    { url = url, hash = hash }
                            in
                            { model
                                | meme = { meme | image = image }
                                , triggerImageProperties =
                                    model.triggerImageProperties + 1
                                , dialog = NoDialog
                            }
                                |> withNoCmd

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
                    (if returnedFile.canDownload then
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
                , showMemeImage = True
                , memeImageUrl = Just url
            }
                |> withCmd
                    (put pK.shownimageurl
                        (Just <| JE.string url)
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
                        (put pK.shownimageurl Nothing)

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
            { model | inputs = { inputs | fileName = name } }
                |> withNoCmd

        SetShowAllImages showAllImages ->
            { model | inputs = { inputs | showAllImages = showAllImages } }
                |> withNoCmd

        SetStorageText storageText ->
            { model | storageText = storageText }
                |> withNoCmd

        LoadStorageMirror ->
            { model
                | loadedStorage = StorageMirror [] []
                , expectedStorageKeys = { memes = -1, images = -1 }
                , storageText = "Loading..."
            }
                |> withCmds
                    [ listKeysLabeled labels.listStorageMemes
                        (pK.memes ++ ".")
                    , listKeysLabeled labels.listStorageImages
                        (pK.imageurls ++ ".")
                    ]

        ReceiveStorageMemeKeys keys ->
            ( model
                |> modifyExpectedStorageKeys (List.length keys + 1) 0
            , keys
                |> List.map
                    (\key ->
                        getLabeled labels.storageMeme key
                    )
                |> Cmd.batch
            )

        ReceiveStorageImageKeys keys ->
            ( model
                |> modifyExpectedStorageKeys 0 (List.length keys + 1)
            , keys
                |> List.map
                    (\key ->
                        getLabeled labels.storageImage key
                    )
                |> Cmd.batch
            )

        ReceiveStorageMeme name value ->
            case value of
                Nothing ->
                    model
                        |> modifyExpectedStorageKeys -1 0
                        |> withCmd checkStorageDone

                Just v ->
                    case ED.decodeMeme v of
                        Err _ ->
                            model
                                |> modifyExpectedStorageKeys -1 0
                                |> withCmd checkStorageDone

                        Ok meme ->
                            let
                                storage =
                                    model.loadedStorage
                            in
                            { model
                                | loadedStorage =
                                    { storage
                                        | memes = ( name, meme ) :: storage.memes
                                    }
                            }
                                |> withCmd checkStorageDone

        ReceiveStorageImage hash value ->
            case value of
                Nothing ->
                    model
                        |> modifyExpectedStorageKeys 0 -1
                        |> withCmd checkStorageDone

                Just v ->
                    case JD.decodeValue JD.string v of
                        Err _ ->
                            model
                                |> modifyExpectedStorageKeys 0 -1
                                |> withCmd checkStorageDone

                        Ok url ->
                            let
                                storage =
                                    model.loadedStorage
                            in
                            { model
                                | loadedStorage =
                                    { storage
                                        | images = ( hash, url ) :: storage.images
                                    }
                            }
                                |> withCmd checkStorageDone

        CheckStorageDone ->
            let
                expected =
                    model.expectedStorageKeys

                storage =
                    model.loadedStorage

                got =
                    { memes = List.length storage.memes
                    , images = List.length storage.images
                    }

                mdl =
                    if got == expected then
                        { model
                            | storageText =
                                ED.encodeStorageMirror storage
                                    |> JE.encode 2
                            , loadedStorage = StorageMirror [] []
                            , expectedStorageKeys =
                                { memes = -1, images = -1 }
                        }

                    else
                        model
            in
            mdl |> withNoCmd

        StoreStorageMirror ->
            case JD.decodeString ED.storageMirrorDecoder model.storageText of
                Err _ ->
                    { model
                        | storageText = "Malformed JSON."
                    }
                        |> withNoCmd

                Ok storage ->
                    { model
                        | storageText = ""
                        , dialog = NoDialog
                    }
                        |> withCmds
                            (List.concat
                                [ List.map
                                    (\( hash, _ ) ->
                                        putImageFlag hash
                                    )
                                    storage.images
                                , List.map
                                    (\( hash, url ) ->
                                        putImage hash url
                                    )
                                    storage.images
                                , List.map
                                    (\( name, meme ) ->
                                        putStorageMeme name meme
                                    )
                                    storage.memes
                                , [ Task.perform (WaitForStore 0) Time.now ]
                                ]
                            )

        WaitForStore goal now ->
            let
                millis =
                    Time.posixToMillis now
            in
            if goal == 0 then
                model
                    |> withCmd
                        (Task.perform (WaitForStore <| millis + 500) Time.now)

            else if millis >= goal then
                model
                    |> withCmd
                        (listKeys <| pK.memes ++ ".")

            else
                -- I hate busy-waiting but changing Time.every is broken
                model
                    |> withCmd
                        (Task.perform (WaitForStore goal) Time.now)

        CloseDataDialog ->
            { model
                | storageText = ""
                , dialog = NoDialog
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

        SequenceDone wrapper ->
            wrapper model

        ProcessLocalStorage value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    funnelState
                    model
            of
                Err error ->
                    { model | msg = Just error } |> withNoCmd

                Ok res ->
                    res


checkStorageDone : Cmd Msg
checkStorageDone =
    Task.perform identity <| Task.succeed CheckStorageDone


modifyExpectedStorageKeys : Int -> Int -> Model -> Model
modifyExpectedStorageKeys memeInc imageInc model =
    let
        keys =
            model.expectedStorageKeys
    in
    { model
        | expectedStorageKeys =
            { keys
                | memes = keys.memes + memeInc
                , images = keys.images + imageInc
            }
    }


setImagesDialog : Model -> ( Model, Cmd Msg )
setImagesDialog model =
    let
        ( mdl, cmd ) =
            { model
                | dialog = ImagesDialog
                , loadedImages = []
            }
                |> listMemes
    in
    mdl |> withCmds [ cmd, listImages ]


loadThumbnails : Model -> ( Model, Cmd Msg )
loadThumbnails model =
    let
        neededThumbnails =
            Dict.keys model.thumbnails
                |> Set.fromList
                |> Set.diff model.knownImages
                |> Set.toList
    in
    case neededThumbnails of
        [] ->
            model |> withNoCmd

        hash :: rest ->
            { model | neededThumbnails = rest }
                |> withCmd (getThumbnail hash)


adjoin : a -> List a -> List a
adjoin a list =
    if List.member a list then
        list

    else
        a :: list


loadNextMemeImage : Model -> ( Model, Cmd Msg )
loadNextMemeImage model =
    case model.neededMemes of
        [] ->
            model |> withNoCmd

        name :: tail ->
            { model | neededMemes = tail }
                |> withCmd (getMemeImage name)


loadNextThumbnail : Model -> ( Model, Cmd Msg )
loadNextThumbnail model =
    case model.neededThumbnails of
        [] ->
            { model | thumbnailImageUrl = "" } |> withNoCmd

        hash :: tail ->
            { model | neededThumbnails = tail }
                |> withCmd (getThumbnail hash)


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


getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort ProcessLocalStorage moduleName False


localStorageSend : LocalStorage.Message -> model -> Cmd Msg
localStorageSend message model =
    LocalStorage.send (getCmdPort LocalStorage.moduleName model)
        message
        funnelState.storage


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    case Debug.log "storageHandler" response of
        LocalStorage.GetResponse { label, key, value } ->
            let
                ( pkey, subkey ) =
                    decodeSubkey key
            in
            if pkey == pK.images then
                -- images exists flag, indexed as "images.<hash>"
                ( model
                , Task.perform (ReceiveImageKey subkey) (Task.succeed value)
                )

            else if pkey == pK.meme then
                -- current Meme, indexed as "meme>"
                ( model
                , Task.perform ReceiveMeme (Task.succeed value)
                )

            else if pkey == pK.model then
                -- SavedModel, indexed as "model"
                ( model
                , Task.perform ReceiveModel (Task.succeed value)
                )

            else if pkey == pK.imageurls then
                -- "data:..." URL for image, indexes as "imageurls.<hash>"
                let
                    msg =
                        if Just labels.imageForThumbnail == label then
                            ReceiveImageForThumbnail subkey

                        else if Just labels.imageFromDialog == label then
                            ReceiveImageFromDialog subkey

                        else if Just labels.storageImage == label then
                            ReceiveStorageImage subkey

                        else
                            ReceiveImage subkey
                in
                model
                    |> withCmd (Task.perform msg <| Task.succeed value)

            else if pkey == pK.memes then
                -- Meme, indexed as "memes.<saved name>"
                let
                    msg =
                        if Just labels.memeImage == label then
                            ReceiveMemeImageHash subkey

                        else if Just labels.storageMeme == label then
                            ReceiveStorageMeme subkey

                        else
                            ReceiveMeme
                in
                model
                    |> withCmd (Task.perform msg <| Task.succeed value)

            else if pkey == pK.thumbnails then
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
                    (if label == Just labels.listImageUrls then
                        ReceiveImageUrls

                     else if label == Just labels.listStorageMemes then
                        ReceiveStorageMemeKeys

                     else if label == Just labels.listStorageImages then
                        ReceiveStorageImageKeys

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
            , button [ onClick <| SetDialog DataDialog ]
                [ text "Data" ]
            , button [ onClick <| SetDialog MemesDialog ]
                [ text "Memes" ]
            , button [ onClick <| SetImagesDialog ]
                [ text "Images" ]
            , case model.thumbnailImageUrl of
                "" ->
                    text ""

                url ->
                    let
                        ( w, h ) =
                            model.thumbnailImageSize
                    in
                    p [ style "display" "none" ]
                        [ svg
                            [ Svg.Attributes.id thumbnailSvgId
                            , width <| tos w
                            , height <| tos h
                            ]
                            [ Svg.Lazy.lazy3 renderThumbnailImage w h url ]
                        ]
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
            , ImageProperties.imageProperties
                [ ImageProperties.imageId thumbnailImageId
                , ImageProperties.triggerImageProperties
                    model.triggerThumbnailProperties
                , ImageProperties.onImageProperties ReceiveThumbnailProperties
                ]
                []
            , SvgToDataUrl.svgToDataUrl
                [ SvgToDataUrl.returnedUrlParameters thumbnailSvgId "image/jpeg"
                , SvgToDataUrl.triggerReturnedUrl model.triggerThumbnailUrl
                , SvgToDataUrl.onReturnedUrl
                    (\returnedUrl ->
                        ReceiveThumbnailUrl returnedUrl.url
                    )
                ]
                []
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
                , b "Width: "
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
                , b "Size:"
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
Welcome to ZAP Meme!

ZAP Meme is a webapp that runs entirely in your browser. It **NEVER**
sends anything back to the server, except requests for the code that
makes it work, so none of your chosen images or text will ever be
known, unless you save a meme you make, and post it somewhere public.

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
color specification in the text area to its right. E.g. "#8f0" for a
bright green (a little red mixed in).

If you choose an "Outline Color", the text will stand out better from
the background image. Again, you may enter a custom color. I find
"impact" and "arial-black" to be the best-looking outlined fonts.

Click "Hide Help" to hide this text, and "Show Help" to show it again.

Click "Memes" to bring up a dialog where you can save the current
meme, or load or delete previously saved memes. The "Save Meme" button
there will save the current meme with the name in the text box to its
left. Clicking an "O" button in the "Load" column (or the underlined
meme name) will load that meme. Clicking an "X" button in the "Delete"
column will delete that meme. The images will remain, so you'll only
lose the meme text and sizing information.

Click "Images" to bring up a dialog showing all images you have ever
used with ZAP Meme in the current browser. They will be sorted by the
name of the first meme for each image, with images that have no saved
memes first, in random order. If you uncheck "Show all images", only
images with saved memes will be shown. If you click the "X" button in
the "Delete" column for an image row, that image, and all saved memes
that use it, will be removed from your browser's database. There is no
confirmation and no undo. Be careful out there.

Click "Data" to bring up a dialog with two buttons. "Load" loads all
memes and images from your browser's database, and encodes them as a
long string you can paste into another browser to "Store" them into
its database. This can take a few seconds. You can load a subset of
the database on the "Images" dialog, by checking the "Load" column
boxes, and clicking the "Load Checked" button. If the data you "Store"
includes a meme with the same name as one of your existing memes, the
old one will be overwritten and lost.

You can dismiss a pop-up dialog either by clicking the "Close" button
at the bottom, or by pressing the "Esc" key on your keyboard.
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


thumbnailScaledHeight : Int
thumbnailScaledHeight =
    50


thumbnailImageWidth : Int
thumbnailImageWidth =
    144


thumbnailImageHeight : Int
thumbnailImageHeight =
    100


renderThumbnailImage : Int -> Int -> String -> Svg Msg
renderThumbnailImage w h url =
    Svg.image
        [ Svg.Attributes.id thumbnailImageId
        , width <| tos w
        , height <| tos h
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
        [ td []
            [ a
                [ href "#"
                , onClick <| LoadSavedMeme name
                ]
                [ text name ]
            ]
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


imagesDialog : Model -> Config Msg
imagesDialog model =
    let
        imageList =
            Dict.toList model.thumbnails
                |> List.map Tuple.second

        showAllImages =
            model.inputs.showAllImages

        rows =
            List.map (savedImageRow model) imageList
                |> List.sortBy Tuple.first
                |> List.filter
                    (if showAllImages then
                        \_ -> True

                     else
                        \( memes, _ ) -> memes /= []
                    )
                |> List.map Tuple.second
    in
    { styles = []
    , title = "Images"
    , content =
        [ input
            [ type_ "checkbox"
            , onCheck SetShowAllImages
            , checked model.inputs.showAllImages
            ]
            []
        , text " Show all images "
        , button
            [ onClick LoadCheckedImages
            , disabled <| model.loadedImages == []
            ]
            [ text "Load Checked" ]
        , br
        , table
            []
            (tr []
                [ thead "Image"
                , thead "Delete"
                , thead "Load"
                , thead "Memes"
                ]
                :: rows
            )
        ]
    , actionBar = [ dismissDialogButton ]
    }


savedImageRow : Model -> Image -> ( List String, Html Msg )
savedImageRow model image =
    let
        names =
            Dict.get image.hash model.imageMemes
                |> Maybe.withDefault []
                |> List.sort
    in
    ( names
    , tr []
        [ td
            [ align "center"
            ]
            [ a
                [ href "#"
                , onClick <| GetImageFromDialog image.hash
                ]
                [ img
                    [ src image.url
                    , alt image.hash
                    , height <| tos thumbnailScaledHeight
                    ]
                    []
                ]
            ]
        , td [ align "center" ]
            [ button
                [ onClick <| DeleteSavedImage image.hash
                , disabled <| image.hash == initialMeme.image.hash
                ]
                [ text "X" ]
            ]
        , td [ align "center" ]
            [ input
                [ type_ "checkbox"
                , onCheck <| CheckLoadedImage image.hash
                , checked <| List.member image.hash model.loadedImages
                ]
                []
            ]
        , td []
            (List.map loadMemeLink names
                |> List.intersperse br
            )
        ]
    )


loadMemeLink : String -> Html Msg
loadMemeLink name =
    a
        [ href "#"
        , onClick <| LoadSavedMeme name
        ]
        [ text name ]


dataDialog : Model -> Config Msg
dataDialog model =
    { styles = []
    , title = "Data"
    , content =
        [ button
            [ onClick LoadStorageMirror ]
            [ text "Load" ]
        , text " "
        , button
            [ onClick StoreStorageMirror ]
            [ text "Store" ]
        , p []
            [ textarea
                [ onInput SetStorageText
                , rows 4
                , value model.storageText
                ]
                [ text model.storageText ]
            ]
        ]
    , actionBar =
        [ button [ onClick CloseDataDialog ]
            [ text "Close" ]
        ]
    }


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
    let
        config =
            case model.dialog of
                HelpDialog ->
                    helpDialog model

                MemesDialog ->
                    memesDialog model

                ImagesDialog ->
                    imagesDialog model

                DataDialog ->
                    dataDialog model

                _ ->
                    noDialog model

        ( _, h ) =
            model.windowSize
    in
    { config
        | styles =
            List.append
                [ ( "max-height", (tos <| h * 85 // 100) ++ "px" )
                , ( "overflow", "auto" )
                ]
                config.styles
    }



{-

      Persistence

   Keys are in `pK`.

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


dummySavedModel : SavedModel
dummySavedModel =
    { dialog = NoDialog
    , selectedPosition = Nothing
    , savedSelectedPosition = Nothing
    , showCaptionBorders = False
    , maxWidth = 800
    , maxHeight = 600
    , inputs = initialInputs
    , showMemeImage = False
    , showHelp = False
    }


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


clear : String -> Cmd Msg
clear prefix =
    localStorageSend (LocalStorage.clear <| Debug.log "clear" prefix) ()


get : String -> Cmd Msg
get key =
    localStorageSend (LocalStorage.get <| Debug.log "get" key) ()


getLabeled : String -> String -> Cmd Msg
getLabeled label key =
    localStorageSend
        (LocalStorage.getLabeled label <|
            Debug.log ("getLabeled " ++ label) key
        )
        ()


put : String -> Maybe Value -> Cmd Msg
put key value =
    let
        k =
            Debug.log "put" key
    in
    localStorageSend (LocalStorage.put key value) ()


justTrueValue : Maybe Value
justTrueValue =
    Just <| JE.bool True


putImageFlag : String -> Cmd Msg
putImageFlag hash =
    let
        key =
            encodeSubkey pK.images hash
    in
    put key justTrueValue


putImage : String -> String -> Cmd Msg
putImage hash url =
    let
        key =
            encodeSubkey pK.imageurls hash
    in
    put key (Just <| JE.string url)


putStorageMeme : String -> Meme -> Cmd Msg
putStorageMeme name meme =
    let
        key =
            encodeSubkey pK.memes name
    in
    put key (Just <| ED.encodeMeme meme)


listKeys : String -> Cmd Msg
listKeys prefix =
    localStorageSend (LocalStorage.listKeys <| Debug.log "listKeys" prefix) ()


listKeysLabeled : String -> String -> Cmd Msg
listKeysLabeled label prefix =
    localStorageSend
        (LocalStorage.listKeysLabeled label <|
            Debug.log ("listKeysLabeled " ++ label) prefix
        )
        ()


putModel : Model -> Cmd Msg
putModel model =
    let
        savedModel =
            modelToSavedModel model

        value =
            ED.encodeSavedModel savedModel
    in
    Cmd.batch
        [ put pK.model (Just value)
        , putMeme model.meme
        ]


putMeme : Meme -> Cmd Msg
putMeme meme =
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
        [ put pK.meme (Just value)
        , Task.perform (MaybePutImageUrl urlHash) <| Task.succeed url
        ]


putSavedMeme : String -> Maybe Meme -> Cmd Msg
putSavedMeme name meme =
    let
        key =
            encodeSubkey pK.memes name

        value =
            case meme of
                Nothing ->
                    Nothing

                Just m ->
                    Just <| ED.encodeMeme m
    in
    put key value


getSavedMeme : String -> Cmd Msg
getSavedMeme name =
    let
        key =
            encodeSubkey pK.memes name
    in
    get key


getMemeImage : String -> Cmd Msg
getMemeImage name =
    let
        key =
            encodeSubkey pK.memes name
    in
    getLabeled labels.memeImage key


listMemes : Model -> ( Model, Cmd Msg )
listMemes model =
    let
        loadedMemes =
            Dict.toList model.memeImages
                |> List.map Tuple.first
                |> Set.fromList

        neededMemes =
            Set.diff model.savedMemes loadedMemes
    in
    case Set.toList neededMemes of
        [] ->
            model |> withNoCmd

        names ->
            { model | neededMemes = names }
                |> loadNextMemeImage


listImages : Cmd Msg
listImages =
    listKeysLabeled labels.listImageUrls
        (pK.imageurls ++ ".")


getThumbnail : String -> Cmd Msg
getThumbnail hash =
    let
        key =
            encodeSubkey pK.thumbnails hash
    in
    get key


putThumbnail : String -> String -> Cmd Msg
putThumbnail hash url =
    let
        key =
            encodeSubkey pK.thumbnails hash
    in
    put key (Just <| JE.string url)



{-

   State machines for LocalStorage.

   See state-machines.md.

-}


newLabels =
    { -- Simple
      saveImage = "saveImage"
    , getImage = "getImage"

    -- Complex
    , startup = "startup"
    , prepareImages = "prepareImages"
    , loadData = "loadData"
    }


type StorageState
    = SaveImageState
        { url : String
        , hash : String
        }
    | GetImageState String
    | StartupState
        { model : Maybe SavedModel
        , meme : Maybe Meme
        , image : Maybe Image
        , shownUrl : Maybe String
        }
    | PrepareImagesDialogState
        { mode : PrepareImagesDialogMode
        , hashes : List String
        , thumbnails : Dict String Image -- hash -> image
        , names : List String
        , memeImage : Dict String String -- name -> hash
        , imageMemes : Dict String (List String) -- hash -> names
        }
    | LoadDataState
        { mode : LoadDataMode
        , hashes : List String
        , names : List String
        , images : List Image
        , memes : List Meme
        }


type PrepareImagesDialogMode
    = PrepareImagesIdle
    | PrepareImagesListImages
    | PrepareImagesLoadThumbnail
    | PrepareImagesLoadImage
    | PrepareImagesComputeSize
    | PrepareImagesGetThumbnail
    | PrepareImagesListMemes
    | PrepareImagesLoadMeme


type LoadDataMode
    = LoadDataIdle
    | LoadDataListImages
    | LoadDataListMemes
    | LoadDataLoadImage
    | LoadDataLoadMeme


type alias LocalStorageStates =
    { saveImage : Sequence.State StorageState Msg
    , getImage : Sequence.State StorageState Msg
    , startup : Sequence.State StorageState Msg
    , prepareImages : Sequence.State StorageState Msg
    , loadData : Sequence.State StorageState Msg
    }


sequenceSender : LocalStorage.Message -> Cmd Msg
sequenceSender message =
    localStorageSend message ()


initialStorageStates : LocalStorageStates
initialStorageStates =
    { saveImage =
        { state = SaveImageState { url = "", hash = "" }
        , label = newLabels.saveImage
        , process = saveImageStateProcess
        , sender = sequenceSender
        }
    , getImage =
        { state = GetImageState ""
        , label = newLabels.getImage
        , process = getImageStateProcess
        , sender = sequenceSender
        }
    , startup =
        { state = initialStartupState
        , label = newLabels.startup
        , process = startupStateProcess
        , sender = sequenceSender
        }
    , prepareImages =
        { state = initialPrepareImagesDialogState
        , label = newLabels.prepareImages
        , process = prepareImagesStateProcess
        , sender = sequenceSender
        }
    , loadData =
        { state = initialLoadDataState
        , label = newLabels.loadData
        , process = loadDataStateProcess
        , sender = sequenceSender
        }
    }


{-| This is the reason I wrote `PortFunnel.LocalStorage.Sequence`.

To add a new process, you just have to add it to to `newLabels`,
`StorageState`, `LocalStorageStates`, & `initialStorageStates`, write
start, processing, and done functions, and add an element to the
`Sequence.multiProcess` call below.

Mostly data driven, as an old lisper likes it.

-}
newStorageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
newStorageHandler response state model =
    let
        states =
            model.localStorageStates
    in
    case
        Sequence.multiProcess
            response
            [ ( states.saveImage
              , \res -> { states | saveImage = res }
              )
            , ( states.getImage
              , \res -> { states | getImage = res }
              )
            , ( states.startup
              , \res -> { states | startup = res }
              )
            , ( states.prepareImages
              , \res -> { states | prepareImages = res }
              )
            , ( states.loadData
              , \res -> { states | loadData = res }
              )
            ]
    of
        Just ( res, setter, cmd ) ->
            { model | localStorageStates = setter res }
                |> withCmd cmd

        _ ->
            model |> withNoCmd


type alias DbRequest =
    Sequence.DbRequest Msg


startSaveImage : String -> String -> Model -> ( Model, Cmd Msg )
startSaveImage hash url model =
    let
        pair =
            { prefix = pK.images
            , subkey = hash
            }

        states =
            model.localStorageStates

        saveImageState =
            states.saveImage

        state2 =
            { saveImageState
                | state = SaveImageState { url = url, hash = hash }
            }
    in
    ( { model
        | localStorageStates =
            { states
                | saveImage =
                    state2
            }
      }
    , Sequence.send (DbGet pair) state2
    )


saveImageStateProcess : DbResponse -> StorageState -> ( DbRequest, StorageState )
saveImageStateProcess response state =
    let
        nullReturn =
            ( DbNothing, state )
    in
    case state of
        SaveImageState { url, hash } ->
            case Sequence.decodeExpectedDbGot JD.bool hash response of
                Just ( _, Just True ) ->
                    nullReturn

                Just ( pair, _ ) ->
                    ( DbPut pair <| JE.string url
                    , state
                    )

                _ ->
                    nullReturn

        _ ->
            nullReturn


startGetImage : String -> Model -> ( Model, Cmd Msg )
startGetImage hash model =
    let
        pair =
            { prefix = pK.imageurls
            , subkey = hash
            }

        localStorageStates =
            model.localStorageStates

        getImageState =
            localStorageStates.getImage

        state2 =
            { getImageState | state = GetImageState hash }
    in
    ( { model
        | localStorageStates =
            { localStorageStates | getImage = state2 }
      }
    , Sequence.send (DbGet pair) state2
    )


getImageStateProcess : DbResponse -> StorageState -> ( DbRequest, StorageState )
getImageStateProcess response state =
    let
        nullReturn =
            ( DbNothing, state )
    in
    case state of
        GetImageState hash ->
            case Sequence.decodeExpectedDbGot JD.string hash response of
                Just ( pair, Just url ) ->
                    ( DbCustomRequest <|
                        Task.perform SequenceDone
                            (Task.succeed <| getImageDone hash url)
                    , state
                    )

                _ ->
                    nullReturn

        _ ->
            nullReturn


getImageDone : String -> String -> Model -> ( Model, Cmd Msg )
getImageDone hash url model =
    let
        meme =
            model.meme
    in
    { model
        | showMemeImage = False
        , meme = { meme | image = { url = url, hash = hash } }
        , triggerImageProperties = model.triggerImageProperties + 1
    }
        |> withNoCmd


initialStartupState : StorageState
initialStartupState =
    StartupState
        { model = Nothing
        , meme = Nothing
        , image = Nothing
        , shownUrl = Nothing
        }


startStartup : Model -> ( Model, Cmd Msg )
startStartup model =
    let
        getModelPair =
            { prefix = pK.model
            , subkey = ""
            }

        getShownImagePair =
            { prefix = pK.shownimageurl
            , subkey = ""
            }

        localStorageStates =
            model.localStorageStates

        startupState =
            localStorageStates.startup

        state2 =
            { startupState | state = initialStartupState }
    in
    { model
        | localStorageStates =
            { localStorageStates | startup = state2 }
    }
        |> withCmd
            (Sequence.send
                (DbGet <|
                    if model.showMemeImage then
                        getModelPair

                    else
                        getShownImagePair
                )
                state2
            )


dbResponsePrefix : DbResponse -> String
dbResponsePrefix response =
    case response of
        DbGot { prefix } _ ->
            prefix

        _ ->
            ""


startupStateProcess : DbResponse -> StorageState -> ( DbRequest, StorageState )
startupStateProcess response state =
    let
        nullReturn =
            ( DbNothing, state )
    in
    case state of
        StartupState startupState ->
            let
                prefix =
                    dbResponsePrefix response

                noPair =
                    KeyPair "" ""

                abortTriplet =
                    ( True, startupState, noPair )

                ( done, startupState2, nextPair ) =
                    if prefix == pK.shownimageurl then
                        case Sequence.decodeExpectedDbGot JD.string "" response of
                            Just ( _, Just url ) ->
                                ( False
                                , { startupState | shownUrl = Just url }
                                , KeyPair pK.model ""
                                )

                            _ ->
                                -- If this fails, don't abort.
                                ( False
                                , startupState
                                , KeyPair pK.model ""
                                )

                    else if prefix == pK.model then
                        case Sequence.decodeExpectedDbGot ED.savedModelDecoder "" response of
                            Just ( _, Just model ) ->
                                ( False
                                , { startupState | model = Just model }
                                , KeyPair pK.meme ""
                                )

                            _ ->
                                abortTriplet

                    else if prefix == pK.meme then
                        case Sequence.decodeExpectedDbGot ED.memeDecoder "" response of
                            Just ( _, Just meme ) ->
                                ( False
                                , { startupState | meme = Just meme }
                                , KeyPair pK.imageurls meme.image.hash
                                )

                            _ ->
                                abortTriplet

                    else if prefix == pK.imageurls then
                        case Sequence.decodeExpectedDbGot JD.string "" response of
                            Just ( { subkey }, Just url ) ->
                                ( True
                                , { startupState
                                    | image = Just { url = url, hash = subkey }
                                  }
                                , noPair
                                )

                            _ ->
                                abortTriplet

                    else
                        abortTriplet
            in
            ( if done then
                DbCustomRequest <|
                    Task.perform SequenceDone
                        (Task.succeed startupDone)

              else
                DbGet nextPair
            , StartupState startupState2
            )

        -- `state` is not a `StartupState`. Shouldn't happen, but ignore
        _ ->
            nullReturn


startupDone : Model -> ( Model, Cmd Msg )
startupDone model =
    let
        states =
            model.localStorageStates

        startupState =
            states.startup
    in
    case startupState.state of
        StartupState state ->
            case ( ( state.model, state.meme, state.image ), state.shownUrl ) of
                ( ( Just savedModel, Just meme, Just image ), shownUrl ) ->
                    let
                        mdl =
                            savedModelToModel savedModel model

                        mdl2 =
                            { mdl
                                | showMemeImage = shownUrl /= Nothing
                                , memeImageUrl = shownUrl
                                , meme =
                                    { meme | image = image }
                                , localStorageStates =
                                    { states
                                        | startup =
                                            { startupState
                                                | state = initialStartupState
                                            }
                                    }
                            }
                    in
                    mdl2 |> withNoCmd

                _ ->
                    model |> withNoCmd

        _ ->
            model |> withNoCmd


initialPrepareImagesDialogState : StorageState
initialPrepareImagesDialogState =
    PrepareImagesDialogState
        { mode = PrepareImagesIdle
        , hashes = []
        , thumbnails = Dict.empty
        , names = []
        , memeImage = Dict.empty
        , imageMemes = Dict.empty
        }


prepareImagesStateProcess : DbResponse -> StorageState -> ( DbRequest, StorageState )
prepareImagesStateProcess response state =
    -- TODO
    ( DbNothing, state )


initialLoadDataState : StorageState
initialLoadDataState =
    LoadDataState
        { mode = LoadDataIdle
        , hashes = []
        , names = []
        , images = []
        , memes = []
        }


loadDataStateProcess : DbResponse -> StorageState -> ( DbRequest, StorageState )
loadDataStateProcess response state =
    -- TODO
    ( DbNothing, state )


{-| Old labels. Will be replaced by `newLabels`
-}
labels =
    { imageForThumbnail = "imageForThumbnail"
    , imageFromDialog = "imageFromDialog"
    , listImageUrls = "listImageUrls"
    , memeImage = "memeImage"
    , listStorageMemes = "listStorageMemes"
    , listStorageImages = "listtorageImages"
    , storageMeme = "storageMeme"
    , storageImage = "storageImage"
    }


{-| Plural means there are subkeys, e.g. "memes.<name>","images.<hash>"
-}
pK =
    { model = "model"
    , meme = "meme"
    , shownimageurl = "shownimageurl"
    , memes = "memes"
    , images = "images"
    , imageurls = "imageurls"
    , thumbnails = "thumbnails"
    }
