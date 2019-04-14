---------------------------------------------------------------------
--
-- Sequencer.elm
-- PortFunnel.LocalStorage.Sequence state machines for ZapMeme
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ZapMeme.Sequencer exposing
    ( LocalStorageStates
    , Wrappers
    , initialStorageStates
    , injectThumbnail
    )

{-

   State machines for LocalStorage.

   See state-machines.md.

-}

import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import CustomElement.ImageProperties as ImageProperties exposing (ImageProperties)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.LocalStorage.Sequence as Sequence
    exposing
        ( DbRequest(..)
        , DbResponse(..)
        , KeyPair
        )
import PortFunnels
import Task
import ZapMeme.EncodeDecode as ED
import ZapMeme.Types exposing (Image, Meme, SavedModel)


{-| pK is short for persistenceKeys. I got tired of typing it.

Plural means there are subkeys, e.g. "memes.<name>","images.<hash>"

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


labels =
    { -- Simple
      saveImage = "saveImage"
    , getImage = "getImage"

    -- Complex
    , startup = "startup"
    , prepareImages = "prepareImages"
    , loadData = "loadData"
    }


type StorageState model msg
    = SaveImageState
        { url : String
        , hash : String
        }
    | GetImageState
        { hash : String
        , receiver : Image -> model -> ( model, Cmd msg )
        , wrappers : Wrappers model msg
        }
    | StartupState
        { model : Maybe SavedModel
        , meme : Maybe Meme
        , image : Maybe Image
        , shownUrl : Maybe String
        , receiver : SavedModel -> Meme -> Maybe String -> model -> ( model, Cmd msg )
        , wrappers : Wrappers model msg
        }
    | PrepareImagesDialogState
        { hashes : List KeyPair
        , thumbnails : Dict String Image -- hash -> image
        , names : List KeyPair
        , memeImage : Dict String String -- name -> hash
        , imageMemes : Dict String (List String) -- hash -> names
        , imageSizeGetter : String -> String -> model -> ( model, Cmd msg )
        , wrappers : Wrappers model msg
        }
    | LoadDataState
        { hashes : List String
        , names : List String
        , images : List Image
        , memes : List Meme
        , receiver : List Image -> List Meme -> model -> ( model, Cmd msg )
        , wrappers : Wrappers model msg
        }


{-| This is how we communicate with Main.elm
-}
type alias Wrappers model msg =
    { sender : LocalStorage.Message -> Cmd msg
    , injector : Value -> msg
    , localStorageStates : model -> LocalStorageStates model msg
    , setLocalStorageStates : LocalStorageStates model msg -> model -> model
    , sequenceDone : (model -> ( model, Cmd msg )) -> msg
    }


type LocalStorageStates model msg
    = LocalStorageStates
        { wrappers : Wrappers model msg
        , saveImage : Sequence.State (StorageState model msg) msg
        , getImage : Sequence.State (StorageState model msg) msg
        , startup : Sequence.State (StorageState model msg) msg
        , prepareImages : Sequence.State (StorageState model msg) msg
        , loadData : Sequence.State (StorageState model msg) msg
        }


initialStorageStates : Wrappers model msg -> LocalStorageStates model msg
initialStorageStates wrappers =
    LocalStorageStates
        { wrappers = wrappers
        , saveImage =
            { state = initialSaveImageState
            , label = labels.saveImage
            , process = saveImageStateProcess
            , sender = wrappers.sender
            }
        , getImage =
            { state = initialGetImageState wrappers
            , label = labels.getImage
            , process = getImageStateProcess
            , sender = wrappers.sender
            }
        , startup =
            { state = initialStartupState wrappers
            , label = labels.startup
            , process = startupStateProcess
            , sender = wrappers.sender
            }
        , prepareImages =
            { state = initialPrepareImagesDialogState wrappers
            , label = labels.prepareImages
            , process = prepareImagesStateProcess
            , sender = wrappers.sender
            }
        , loadData =
            { state = initialLoadDataState wrappers
            , label = labels.loadData
            , process = loadDataStateProcess
            , sender = wrappers.sender
            }
        }


{-| This is the reason I wrote `PortFunnel.LocalStorage.Sequence`.

To add a new process, you just have to add it to to `labels`,
`StorageState`, `LocalStorageStates`, & `initialStorageStates`, write
start, processing, and done functions, and add an element to the
`Sequence.multiProcess` call below.

Mostly data driven, as an old lisper likes it.

-}
storageHandler : Wrappers model msg -> LocalStorage.Response -> PortFunnels.State -> model -> ( model, Cmd msg )
storageHandler wrappers response state model =
    let
        (LocalStorageStates states) =
            wrappers.localStorageStates model
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
            wrappers.setLocalStorageStates (LocalStorageStates <| setter res) model
                |> withCmd cmd

        _ ->
            model |> withNoCmd


initialSaveImageState : StorageState model msg
initialSaveImageState =
    makeSaveImageState "" ""


makeSaveImageState : String -> String -> StorageState model msg
makeSaveImageState url hash =
    SaveImageState
        { url = url
        , hash = hash
        }


startSaveImage : Wrappers model msg -> String -> String -> model -> ( model, Cmd msg )
startSaveImage wrappers hash url model =
    let
        pair =
            { prefix = pK.images
            , subkey = hash
            }

        (LocalStorageStates states) =
            wrappers.localStorageStates model

        saveImageState =
            states.saveImage

        state2 =
            { saveImageState
                | state = makeSaveImageState url hash
            }
    in
    ( wrappers.setLocalStorageStates
        (LocalStorageStates { states | saveImage = state2 })
        model
    , Sequence.send state2 (DbGet pair)
    )


saveImageStateProcess : DbResponse -> StorageState model msg -> ( DbRequest msg, StorageState model msg )
saveImageStateProcess response storageState =
    let
        nullReturn =
            ( DbNothing, storageState )
    in
    case storageState of
        SaveImageState { url, hash } ->
            case Sequence.decodeExpectedDbGot JD.bool hash response of
                Just ( _, Just True ) ->
                    nullReturn

                Just ( pair, _ ) ->
                    ( DbPut pair <| JE.string url
                    , storageState
                    )

                _ ->
                    nullReturn

        _ ->
            nullReturn


initialGetImageState : Wrappers model msg -> StorageState model msg
initialGetImageState wrappers =
    makeGetImageState wrappers "" (\_ model -> ( model, Cmd.none ))


makeGetImageState : Wrappers model msg -> String -> (Image -> model -> ( model, Cmd msg )) -> StorageState model msg
makeGetImageState wrappers hash receiver =
    GetImageState
        { hash = hash
        , receiver = receiver
        , wrappers = wrappers
        }


startGetImage : Wrappers model msg -> (Image -> model -> ( model, Cmd msg )) -> String -> model -> ( model, Cmd msg )
startGetImage wrappers receiver hash model =
    let
        pair =
            { prefix = pK.imageurls
            , subkey = hash
            }

        (LocalStorageStates localStorageStates) =
            wrappers.localStorageStates model

        state =
            localStorageStates.getImage

        state2 =
            { state
                | state =
                    makeGetImageState wrappers hash receiver
            }
    in
    ( wrappers.setLocalStorageStates
        (LocalStorageStates { localStorageStates | getImage = state2 })
        model
    , Sequence.send state2 (DbGet pair)
    )


getImageStateProcess : DbResponse -> StorageState model msg -> ( DbRequest msg, StorageState model msg )
getImageStateProcess response storageState =
    let
        nullReturn =
            ( DbNothing, storageState )
    in
    case storageState of
        GetImageState { hash, receiver, wrappers } ->
            case Sequence.decodeExpectedDbGot JD.string hash response of
                Just ( pair, Just url ) ->
                    ( DbCustomRequest <|
                        Task.perform wrappers.sequenceDone
                            (Task.succeed <| getImageDone receiver hash url)
                    , storageState
                    )

                _ ->
                    nullReturn

        _ ->
            nullReturn


getImageDone : (Image -> model -> ( model, Cmd msg )) -> String -> String -> model -> ( model, Cmd msg )
getImageDone receiver hash url model =
    receiver { url = url, hash = hash } model


initialStartupState : Wrappers model msg -> StorageState model msg
initialStartupState wrappers =
    makeStartupState wrappers (\_ _ _ model -> ( model, Cmd.none ))


makeStartupState : Wrappers model msg -> (SavedModel -> Meme -> Maybe String -> model -> ( model, Cmd msg )) -> StorageState model msg
makeStartupState wrappers receiver =
    StartupState
        { model = Nothing
        , meme = Nothing
        , image = Nothing
        , shownUrl = Nothing
        , receiver = receiver
        , wrappers = wrappers
        }


startStartup : Wrappers model msg -> (SavedModel -> Meme -> Maybe String -> model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
startStartup wrappers receiver model =
    let
        getShownImagePair =
            { prefix = pK.shownimageurl
            , subkey = ""
            }

        (LocalStorageStates localStorageStates) =
            wrappers.localStorageStates model

        state =
            localStorageStates.startup

        state2 =
            { state
                | state =
                    makeStartupState wrappers receiver
            }
    in
    wrappers.setLocalStorageStates
        (LocalStorageStates { localStorageStates | startup = state2 })
        model
        |> withCmd
            (Sequence.send state2 <|
                DbGet getShownImagePair
            )


dbResponsePrefix : DbResponse -> String
dbResponsePrefix response =
    case response of
        DbGot { prefix } _ ->
            prefix

        _ ->
            ""


startupStateProcess : DbResponse -> StorageState model msg -> ( DbRequest msg, StorageState model msg )
startupStateProcess response storageState =
    let
        nullReturn =
            ( DbNothing, storageState )
    in
    case storageState of
        StartupState state ->
            let
                prefix =
                    dbResponsePrefix response

                noPair =
                    KeyPair "" ""

                abortTriplet =
                    ( True, state, noPair )

                ( done, state2, nextPair ) =
                    if prefix == pK.shownimageurl then
                        case Sequence.decodeExpectedDbGot JD.string "" response of
                            Just ( _, Just url ) ->
                                ( False
                                , { state | shownUrl = Just url }
                                , KeyPair pK.model ""
                                )

                            _ ->
                                -- no known URL in the store.
                                ( False
                                , state
                                , KeyPair pK.model ""
                                )

                    else if prefix == pK.model then
                        case Sequence.decodeExpectedDbGot ED.savedModelDecoder "" response of
                            Just ( _, Just model ) ->
                                ( False
                                , { state | model = Just model }
                                , KeyPair pK.meme ""
                                )

                            _ ->
                                abortTriplet

                    else if prefix == pK.meme then
                        case Sequence.decodeExpectedDbGot ED.memeDecoder "" response of
                            Just ( _, Just meme ) ->
                                ( False
                                , { state | meme = Just meme }
                                , KeyPair pK.imageurls meme.image.hash
                                )

                            _ ->
                                abortTriplet

                    else if prefix == pK.imageurls then
                        case Sequence.decodeExpectedDbGot JD.string "" response of
                            Just ( { subkey }, Just url ) ->
                                ( True
                                , { state
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
                    Task.perform state2.wrappers.sequenceDone
                        (Task.succeed <|
                            startupDone state2.wrappers state2.receiver
                        )

              else
                DbGet nextPair
            , StartupState state2
            )

        -- `state` is not a `StartupState`. Shouldn't happen, but ignore
        _ ->
            nullReturn


startupDone : Wrappers model msg -> (SavedModel -> Meme -> Maybe String -> model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
startupDone wrappers receiver model =
    let
        (LocalStorageStates states) =
            wrappers.localStorageStates model

        state =
            states.startup
    in
    case state.state of
        StartupState startupState ->
            case
                ( ( startupState.model, startupState.meme, startupState.image )
                , startupState.shownUrl
                )
            of
                ( ( Just savedModel, Just meme, Just image ), shownUrl ) ->
                    let
                        mdl =
                            wrappers.setLocalStorageStates
                                (LocalStorageStates
                                    { states
                                        | startup =
                                            { state
                                                | state =
                                                    initialStartupState wrappers
                                            }
                                    }
                                )
                                model
                    in
                    receiver savedModel { meme | image = image } shownUrl model

                _ ->
                    model |> withNoCmd

        _ ->
            model |> withNoCmd


initialPrepareImagesDialogState : Wrappers model msg -> StorageState model msg
initialPrepareImagesDialogState wrappers =
    makePrepareImagesDialogState wrappers (\_ _ model -> ( model, Cmd.none ))


makePrepareImagesDialogState : Wrappers model msg -> (String -> String -> model -> ( model, Cmd msg )) -> StorageState model msg
makePrepareImagesDialogState wrappers imageSizeGetter =
    PrepareImagesDialogState
        { hashes = []
        , thumbnails = Dict.empty
        , names = []
        , memeImage = Dict.empty
        , imageMemes = Dict.empty
        , imageSizeGetter = imageSizeGetter
        , wrappers = wrappers
        }


startPrepareImages : Wrappers model msg -> (String -> String -> model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
startPrepareImages wrappers imageSizeGetter model =
    let
        pair =
            { prefix = pK.imageurls
            , subkey = "."
            }

        (LocalStorageStates localStorageStates) =
            wrappers.localStorageStates model

        state =
            localStorageStates.prepareImages

        state2 =
            { state
                | state =
                    makePrepareImagesDialogState wrappers imageSizeGetter
            }
    in
    ( wrappers.setLocalStorageStates
        (LocalStorageStates { localStorageStates | prepareImages = state2 })
        model
    , Sequence.send state2 (DbListKeys pair)
    )


prepareImagesStateProcess : DbResponse -> StorageState model msg -> ( DbRequest msg, StorageState model msg )
prepareImagesStateProcess response storageState =
    case storageState of
        PrepareImagesDialogState state ->
            let
                doneTriplet =
                    ( True, state, DbNothing )

                nothingTriplet =
                    ( False, state, DbNothing )

                ( done, state2, request ) =
                    case response of
                        DbKeys { prefix } keys ->
                            if prefix == pK.imageurls then
                                case keys of
                                    [] ->
                                        doneTriplet

                                    key :: rest ->
                                        ( False
                                        , { state | hashes = rest }
                                          -- Get the first thumbnail image
                                        , DbGet { key | prefix = pK.thumbnails }
                                        )

                            else if prefix == pK.memes then
                                case keys of
                                    [] ->
                                        doneTriplet

                                    key :: rest ->
                                        ( False
                                        , { state | names = rest }
                                        , DbGet key
                                        )

                            else
                                -- Can't happen, but no way to proceed.
                                doneTriplet

                        DbGot { prefix, subkey } value ->
                            let
                                nextThumbnail () =
                                    case state.hashes of
                                        [] ->
                                            -- Done getting thumbnails. Get memes.
                                            ( False
                                            , state
                                            , DbGet <| KeyPair pK.memes "."
                                            )

                                        pair :: rest ->
                                            ( False
                                            , { state | hashes = rest }
                                            , DbGet pair
                                            )

                                nextMeme () =
                                    case state.names of
                                        [] ->
                                            ( True
                                            , state
                                            , DbNothing
                                            )

                                        pair :: rest ->
                                            ( False
                                            , { state | names = rest }
                                            , DbGet pair
                                            )
                            in
                            if prefix == pK.thumbnails then
                                case value of
                                    Nothing ->
                                        -- No thumbnail, need to create it.
                                        -- Start by loading the image URL
                                        ( False
                                        , state
                                        , DbGet
                                            { prefix = pK.imageurls
                                            , subkey = subkey
                                            }
                                        )

                                    Just v ->
                                        case JD.decodeValue JD.string v of
                                            Err _ ->
                                                -- Thumbnail corrupt, recreate it
                                                ( False
                                                , state
                                                , DbGet
                                                    { prefix = pK.imageurls
                                                    , subkey = subkey
                                                    }
                                                )

                                            Ok url ->
                                                let
                                                    ( _, state3, req ) =
                                                        nextThumbnail ()
                                                in
                                                ( False
                                                , { state3
                                                    | thumbnails =
                                                        Dict.insert subkey
                                                            { url = url
                                                            , hash = subkey
                                                            }
                                                            state3.thumbnails
                                                  }
                                                , req
                                                )

                            else if prefix == pK.imageurls then
                                case value of
                                    Nothing ->
                                        nextThumbnail ()

                                    Just v ->
                                        case JD.decodeValue JD.string v of
                                            Err _ ->
                                                nextThumbnail ()

                                            Ok url ->
                                                -- Get image size,
                                                -- then make thumbnail,
                                                -- and continue.
                                                ( False
                                                , state
                                                , DbCustomRequest <|
                                                    Task.perform
                                                        state.wrappers.sequenceDone
                                                        (Task.succeed <|
                                                            state.imageSizeGetter
                                                                subkey
                                                                url
                                                        )
                                                )

                            else if prefix == pK.meme then
                                case value of
                                    Nothing ->
                                        nextMeme ()

                                    Just v ->
                                        case JD.decodeValue ED.memeDecoder v of
                                            Err _ ->
                                                nextMeme ()

                                            Ok meme ->
                                                let
                                                    ( done2, state3, req ) =
                                                        nextMeme ()

                                                    name =
                                                        subkey

                                                    hash =
                                                        meme.image.hash

                                                    existingNames =
                                                        Dict.get hash
                                                            state.imageMemes
                                                            |> Maybe.withDefault []

                                                    names =
                                                        name :: existingNames
                                                in
                                                ( done2
                                                , { state3
                                                    | memeImage =
                                                        Dict.insert name
                                                            hash
                                                            state.memeImage
                                                    , imageMemes =
                                                        Dict.insert hash
                                                            names
                                                            state.imageMemes
                                                  }
                                                , req
                                                )

                            else
                                -- Can't happen, but no way to proceed.
                                doneTriplet

                        _ ->
                            -- Not DbKeys or DbGot. Ignore it.
                            ( False, state, DbNothing )
            in
            if done then
                ( DbCustomRequest <|
                    Task.perform state.wrappers.sequenceDone
                        (Task.succeed prepareImagesDone)
                , PrepareImagesDialogState state2
                )

            else
                ( request, PrepareImagesDialogState state2 )

        _ ->
            -- Not PrepareImagesDialogState, ignore it
            ( DbNothing, storageState )


injectThumbnail : Wrappers model msg -> String -> String -> model -> ( model, Cmd msg )
injectThumbnail wrappers url hash model =
    let
        (LocalStorageStates localStorageStates) =
            wrappers.localStorageStates model

        state =
            localStorageStates.prepareImages

        key =
            KeyPair pK.thumbnails hash

        urlValue =
            JE.string url
    in
    model
        |> withCmds
            [ Sequence.inject wrappers.injector state (DbGot key <| Just urlValue)
            , Sequence.send state <| DbPut key urlValue
            ]


prepareImagesDone : model -> ( model, Cmd msg )
prepareImagesDone model =
    -- TODO
    model |> withNoCmd


initialLoadDataState : Wrappers model msg -> StorageState model msg
initialLoadDataState wrappers =
    LoadDataState
        { hashes = []
        , names = []
        , images = []
        , memes = []
        , receiver = \_ _ model -> ( model, Cmd.none )
        , wrappers = wrappers
        }


loadDataStateProcess : DbResponse -> StorageState model msg -> ( DbRequest msg, StorageState model msg )
loadDataStateProcess response storageState =
    -- TODO
    ( DbNothing, storageState )
