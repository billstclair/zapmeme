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
    , StateKey
    , StorageState
    , Wrappers
    , getPrepareImagesData
    , initialMeme
    , initialStorageStates
    , injectThumbnail
    , nullState
    , pK
    , removePrepareImagesImage
    , startGetImage
    , startGetMeme
    , startLoadData
    , startLoadDataForImages
    , startPrepareImages
    , startSaveImage
    , startStartup
    , storageHandler
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
import MD5
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.LocalStorage.Sequence as Sequence
    exposing
        ( DbRequest(..)
        , DbResponse(..)
        , KeyPair
        , LocalStorageStates
        , Wrappers
        )
import PortFunnels
import Task
import ZapMeme.Data exposing (data)
import ZapMeme.EncodeDecode as ED
import ZapMeme.Types
    exposing
        ( Caption
        , Image
        , Meme
        , SavedModel
        , TextAlignment(..)
        , TextPosition(..)
        )


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
    , getMeme = "getMeme"

    -- Complex
    , startup = "startup"
    , prepareImages = "prepareImages"
    , loadData = "loadData"
    }


{-| Unique names for the state machines.
-}
type StateKey
    = SaveImage
    | GetImage
    | GetMeme
    | Startup
    | PrepareImages
    | LoadData


nullState : StorageState model msg
nullState =
    NullState


type StorageState model msg
    = NullState
    | SaveImageState
        { url : String
        , hash : String
        }
    | GetImageState
        { hash : String
        , receiver : Image -> model -> ( model, Cmd msg )
        }
    | GetMemeState
        { name : String
        , meme : Maybe Meme
        , receiver : String -> Meme -> model -> ( model, Cmd msg )
        }
    | StartupState
        { model : Maybe SavedModel
        , meme : Maybe Meme
        , image : Maybe Image
        , shownUrl : Maybe String
        , savedMemes : List String
        , receiver : Maybe SavedModel -> Maybe Meme -> Maybe String -> List String -> model -> ( model, Cmd msg )
        }
      -- There's no receiver here, because Main.imagesDialog
      -- gets the thumbnails and imageMemes directly from here
    | PrepareImagesState
        { hashes : List KeyPair
        , thumbnails : Dict String Image -- hash -> image
        , names : List KeyPair
        , memeImage : Dict String String -- name -> hash
        , imageMemes : Dict String (List String) -- hash -> names
        , imageSizeGetter : String -> String -> model -> ( model, Cmd msg )
        }
    | LoadDataState
        { hashes : List KeyPair
        , names : List KeyPair
        , images : List ( String, String ) --(hash, url)
        , memes : List ( String, Meme ) --(name, meme)
        , receiver : List ( String, String ) -> List ( String, Meme ) -> model -> ( model, Cmd msg )
        }


{-| This is how we communicate with Main.elm
-}
type alias Wrappers model msg =
    Sequence.Wrappers StateKey (StorageState model msg) model msg


type alias LocalStorageStates model msg =
    Sequence.LocalStorageStates StateKey (StorageState model msg) model msg


initialStorageStates : Wrappers model msg -> LocalStorageStates model msg
initialStorageStates wrappers =
    Sequence.makeLocalStorageStates wrappers
        [ ( SaveImage
          , { state = initialSaveImageState
            , label = labels.saveImage
            , process = saveImageStateProcess
            }
          )
        , ( GetImage
          , { state = initialGetImageState
            , label = labels.getImage
            , process = getImageStateProcess
            }
          )
        , ( GetMeme
          , { state = initialGetMemeState
            , label = labels.getMeme
            , process = getMemeStateProcess
            }
          )
        , ( Startup
          , { state = initialStartupState
            , label = labels.startup
            , process = startupStateProcess
            }
          )
        , ( PrepareImages
          , { state = initialPrepareImagesState
            , label = labels.prepareImages
            , process = prepareImagesStateProcess
            }
          )
        , ( LoadData
          , { state = initialLoadDataState
            , label = labels.loadData
            , process = loadDataStateProcess
            }
          )
        ]


{-| This is the reason I wrote `PortFunnel.LocalStorage.Sequence`.

To add a new process, you just have to add it to to `labels`,
`StorageState`, & `initialStorageStates`, and write start, processing, and
done functions.

Mostly data driven, as an old lisper likes it.

-}
storageHandler : Wrappers model msg -> LocalStorage.Response -> PortFunnels.State -> model -> ( model, Cmd msg )
storageHandler wrappers response _ model =
    Sequence.update wrappers model response


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
            KeyPair pK.images hash

        state =
            makeSaveImageState url hash

        ( mdl, saveImageState ) =
            Sequence.setState wrappers SaveImage state model
    in
    ( mdl
    , Sequence.send wrappers saveImageState (DbGet pair)
    )


saveImageStateProcess : Wrappers model msg -> DbResponse -> StorageState model msg -> ( DbRequest msg, StorageState model msg )
saveImageStateProcess wrappers response storageState =
    case storageState of
        SaveImageState { url, hash } ->
            let
                state2 =
                    initialSaveImageState
            in
            case response of
                DbGot pair value ->
                    if value /= Nothing then
                        ( DbNothing, state2 )

                    else
                        ( DbBatch
                            [ DbPut (KeyPair pK.imageurls hash) <| JE.string url
                            , DbPut pair <| JE.bool True
                            ]
                        , state2
                        )

                _ ->
                    ( DbNothing, state2 )

        _ ->
            ( DbNothing, storageState )


initialGetImageState : StorageState model msg
initialGetImageState =
    makeGetImageState "" (\_ model -> ( model, Cmd.none ))


makeGetImageState : String -> (Image -> model -> ( model, Cmd msg )) -> StorageState model msg
makeGetImageState hash receiver =
    GetImageState
        { hash = hash
        , receiver = receiver
        }


startGetImage : Wrappers model msg -> (Image -> model -> ( model, Cmd msg )) -> String -> model -> ( model, Cmd msg )
startGetImage wrappers receiver hash model =
    let
        pair =
            KeyPair pK.imageurls hash

        state =
            makeGetImageState hash receiver

        ( mdl, saveImageState ) =
            Sequence.setState wrappers GetImage state model
    in
    ( mdl
    , Sequence.send wrappers saveImageState (DbGet pair)
    )


getImageStateProcess : Wrappers model msg -> DbResponse -> StorageState model msg -> ( DbRequest msg, StorageState model msg )
getImageStateProcess wrappers response storageState =
    let
        nullReturn =
            ( DbNothing, storageState )
    in
    case storageState of
        GetImageState { hash, receiver } ->
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


initialGetMemeState : StorageState model msg
initialGetMemeState =
    makeGetMemeState "" (\_ _ model -> ( model, Cmd.none ))


makeGetMemeState : String -> (String -> Meme -> model -> ( model, Cmd msg )) -> StorageState model msg
makeGetMemeState name receiver =
    GetMemeState
        { name = name
        , meme = Nothing
        , receiver = receiver
        }


startGetMeme : Wrappers model msg -> (String -> Meme -> model -> ( model, Cmd msg )) -> String -> model -> ( model, Cmd msg )
startGetMeme wrappers receiver name model =
    let
        pair =
            KeyPair pK.memes name

        state =
            makeGetMemeState name receiver

        ( mdl, getMemeState ) =
            Sequence.setState wrappers GetMeme state model
    in
    ( mdl
    , Sequence.send wrappers getMemeState (DbGet pair)
    )


nohash : String
nohash =
    "nohash"


getMemeStateProcess : Wrappers model msg -> DbResponse -> StorageState model msg -> ( DbRequest msg, StorageState model msg )
getMemeStateProcess wrappers response storageState =
    let
        nullReturn =
            ( DbNothing, storageState )
    in
    case storageState of
        GetMemeState state ->
            let
                { name, meme, receiver } =
                    state
            in
            case meme of
                Nothing ->
                    case Sequence.decodeExpectedDbGot ED.memeDecoder name response of
                        Just ( pair, Just gotMeme ) ->
                            ( DbGet <| KeyPair pK.imageurls gotMeme.image.hash
                            , GetMemeState
                                { state | meme = Just gotMeme }
                            )

                        _ ->
                            nullReturn

                Just gotMeme ->
                    case Sequence.decodeExpectedDbGot JD.string "" response of
                        Just ( _, maybeUrl ) ->
                            let
                                image =
                                    gotMeme.image

                                newImage =
                                    case maybeUrl of
                                        Just url ->
                                            { image | url = url }

                                        Nothing ->
                                            { url = "", hash = nohash }

                                filledMeme =
                                    { gotMeme
                                        | image = newImage
                                    }
                            in
                            ( DbCustomRequest <|
                                Task.perform wrappers.sequenceDone
                                    (Task.succeed <|
                                        getMemeDone receiver name filledMeme
                                    )
                            , initialGetMemeState
                            )

                        _ ->
                            nullReturn

        _ ->
            nullReturn


getMemeDone : (String -> Meme -> model -> ( model, Cmd msg )) -> String -> Meme -> model -> ( model, Cmd msg )
getMemeDone receiver name meme model =
    receiver name meme model


initialStartupState : StorageState model msg
initialStartupState =
    makeStartupState (\_ _ _ _ model -> ( model, Cmd.none ))


makeStartupState : (Maybe SavedModel -> Maybe Meme -> Maybe String -> List String -> model -> ( model, Cmd msg )) -> StorageState model msg
makeStartupState receiver =
    StartupState
        { model = Nothing
        , savedMemes = []
        , meme = Nothing
        , image = Nothing
        , shownUrl = Nothing
        , receiver = receiver
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


startStartup : Wrappers model msg -> (Maybe SavedModel -> Maybe Meme -> Maybe String -> List String -> model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
startStartup wrappers receiver model =
    let
        pair =
            KeyPair pK.memes "."

        state =
            makeStartupState receiver

        ( mdl, startupState ) =
            Sequence.setState wrappers Startup state model
    in
    ( mdl
    , Sequence.send wrappers startupState (DbListKeys pair)
    )


dbResponsePrefix : DbResponse -> String
dbResponsePrefix response =
    case response of
        DbGot { prefix } _ ->
            prefix

        DbKeys { prefix } _ ->
            prefix

        _ ->
            ""


startupStateProcess : Wrappers model msg -> DbResponse -> StorageState model msg -> ( DbRequest msg, StorageState model msg )
startupStateProcess wrappers response storageState =
    let
        nullReturn =
            ( DbNothing, storageState )
    in
    case storageState of
        StartupState state ->
            let
                prefix =
                    Debug.log "prefix" <|
                        dbResponsePrefix response

                noPair =
                    KeyPair "" ""

                abortTriplet =
                    ( True, state, noPair )

                ( done, state2, nextPair ) =
                    if prefix == pK.memes then
                        case response of
                            DbKeys _ keys ->
                                ( False
                                , { state | savedMemes = List.map .subkey keys }
                                , KeyPair pK.shownimageurl ""
                                )

                            _ ->
                                abortTriplet

                    else if prefix == pK.shownimageurl then
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
                                -- Show a blank image, instead of failing startup
                                ( True
                                , { state
                                    | image =
                                        Just
                                            { url = "", hash = nohash }
                                  }
                                , noPair
                                )

                    else
                        abortTriplet
            in
            ( if done then
                DbCustomRequest <|
                    Task.perform wrappers.sequenceDone
                        (Task.succeed <| startupDone wrappers)

              else
                DbGet nextPair
            , StartupState state2
            )

        -- `state` is not a `StartupState`. Shouldn't happen, but ignore
        _ ->
            nullReturn


startupDone : Wrappers model msg -> model -> ( model, Cmd msg )
startupDone wrappers model =
    case Sequence.getState wrappers Startup model of
        Nothing ->
            ( model, Cmd.none )

        Just state ->
            case state of
                StartupState startupState ->
                    let
                        mdl =
                            Sequence.setStateOnly wrappers
                                Startup
                                initialStartupState
                                model
                    in
                    case
                        ( ( startupState.model, startupState.meme, startupState.image )
                        , startupState.shownUrl
                        , startupState.savedMemes
                        )
                    of
                        ( ( Just savedModel, Just meme, Just image ), shownUrl, savedMemes ) ->
                            startupState.receiver (Just savedModel)
                                (Just { meme | image = image })
                                shownUrl
                                savedMemes
                                mdl

                        _ ->
                            startupState.receiver Nothing Nothing Nothing [] mdl

                _ ->
                    model |> withNoCmd


initialPrepareImagesState : StorageState model msg
initialPrepareImagesState =
    makePrepareImagesState (\_ _ model -> ( model, Cmd.none ))


makePrepareImagesState : (String -> String -> model -> ( model, Cmd msg )) -> StorageState model msg
makePrepareImagesState imageSizeGetter =
    PrepareImagesState
        { hashes = []
        , thumbnails = Dict.empty
        , names = []
        , memeImage = Dict.empty
        , imageMemes = Dict.empty
        , imageSizeGetter = imageSizeGetter
        }


startPrepareImages : Wrappers model msg -> (String -> String -> model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
startPrepareImages wrappers imageSizeGetter model =
    let
        pair =
            KeyPair pK.imageurls "."

        state =
            makePrepareImagesState imageSizeGetter

        ( mdl, prepareImagesState ) =
            Sequence.setState wrappers PrepareImages state model
    in
    ( mdl
    , Sequence.send wrappers prepareImagesState (DbListKeys pair)
    )


prepareImagesStateProcess : Wrappers model msg -> DbResponse -> StorageState model msg -> ( DbRequest msg, StorageState model msg )
prepareImagesStateProcess wrappers response storageState =
    case storageState of
        PrepareImagesState state ->
            let
                doneTuple =
                    ( state, DbNothing )

                ( state2, request ) =
                    case response of
                        DbKeys { prefix } keys ->
                            if prefix == pK.imageurls then
                                case keys of
                                    [] ->
                                        doneTuple

                                    key :: rest ->
                                        ( { state | hashes = rest }
                                          -- Get the first thumbnail image
                                        , DbGet { key | prefix = pK.thumbnails }
                                        )

                            else if prefix == pK.memes then
                                case keys of
                                    [] ->
                                        doneTuple

                                    key :: rest ->
                                        ( { state | names = rest }
                                        , DbGet key
                                        )

                            else
                                -- Can't happen, but no way to proceed.
                                doneTuple

                        DbGot { prefix, subkey } value ->
                            let
                                nextThumbnail st =
                                    case st.hashes of
                                        [] ->
                                            -- Done getting thumbnails. Get memes.
                                            ( st
                                            , DbListKeys <| KeyPair pK.memes "."
                                            )

                                        pair :: rest ->
                                            if pair.subkey == nohash then
                                                nextThumbnail
                                                    { st | hashes = rest }

                                            else
                                                ( { st | hashes = rest }
                                                , DbGet
                                                    { pair
                                                        | prefix =
                                                            pK.thumbnails
                                                    }
                                                )

                                nextMeme st =
                                    case st.names of
                                        [] ->
                                            ( st, DbNothing )

                                        pair :: rest ->
                                            ( { st | names = rest }
                                            , DbGet pair
                                            )
                            in
                            if prefix == pK.thumbnails then
                                case
                                    Sequence.decodeExpectedDbGot JD.string
                                        ""
                                        response
                                of
                                    Nothing ->
                                        -- Can't happen, response wasn't a DbGot
                                        nextThumbnail state

                                    Just ( _, Nothing ) ->
                                        -- Thumbnail missing or corrupt, recreate it
                                        ( state
                                        , DbGet
                                            { prefix = pK.imageurls
                                            , subkey = subkey
                                            }
                                        )

                                    Just ( _, Just url ) ->
                                        let
                                            ( state3, req ) =
                                                nextThumbnail state
                                        in
                                        ( { state3
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
                                case
                                    Sequence.decodeExpectedDbGot JD.string
                                        ""
                                        response
                                of
                                    Nothing ->
                                        -- Can't happen, response wasn't a DbGot
                                        nextThumbnail state

                                    Just ( _, Nothing ) ->
                                        nextThumbnail state

                                    Just ( _, Just url ) ->
                                        -- Get image size,
                                        -- then make thumbnail,
                                        -- and continue.
                                        ( state
                                        , DbCustomRequest <|
                                            Task.perform
                                                wrappers.sequenceDone
                                                (Task.succeed <|
                                                    state.imageSizeGetter
                                                        subkey
                                                        url
                                                )
                                        )

                            else if prefix == pK.memes then
                                case
                                    Sequence.decodeExpectedDbGot ED.memeDecoder
                                        ""
                                        response
                                of
                                    Nothing ->
                                        -- Can't happen, response wasn't a DbGot
                                        nextMeme state

                                    Just ( _, Nothing ) ->
                                        nextMeme state

                                    Just ( _, Just meme ) ->
                                        let
                                            ( state3, req ) =
                                                nextMeme state

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
                                        ( { state3
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
                                doneTuple

                        _ ->
                            -- Not DbKeys or DbGot. Ignore it.
                            doneTuple
            in
            ( request, PrepareImagesState state2 )

        _ ->
            -- Not PrepareImagesState, ignore it
            ( DbNothing, storageState )


getPrepareImagesData : Wrappers model msg -> model -> ( Dict String Image, Dict String (List String) )
getPrepareImagesData wrappers model =
    let
        empty =
            ( Dict.empty, Dict.empty )
    in
    case Sequence.getState wrappers PrepareImages model of
        Nothing ->
            empty

        Just prepareImagesState ->
            case prepareImagesState of
                PrepareImagesState state ->
                    ( state.thumbnails, state.imageMemes )

                _ ->
                    ( Dict.empty, Dict.empty )


removePrepareImagesImage : Wrappers model msg -> String -> model -> model
removePrepareImagesImage wrappers hash model =
    case Sequence.getState wrappers PrepareImages model of
        Nothing ->
            model

        Just prepareImagesState ->
            case prepareImagesState of
                PrepareImagesState state ->
                    let
                        state2 =
                            PrepareImagesState
                                { state
                                    | thumbnails = Dict.remove hash state.thumbnails
                                    , imageMemes = Dict.remove hash state.imageMemes
                                }
                    in
                    Sequence.setStateOnly wrappers
                        PrepareImages
                        state2
                        model

                _ ->
                    model


injectThumbnail : Wrappers model msg -> String -> String -> model -> ( model, Cmd msg )
injectThumbnail wrappers url hash model =
    case Sequence.getFullState wrappers PrepareImages model of
        Nothing ->
            model |> withNoCmd

        Just state ->
            let
                key =
                    KeyPair pK.thumbnails hash

                urlValue =
                    JE.string url
            in
            model
                |> withCmds
                    [ Sequence.inject wrappers state (DbGot key <| Just urlValue)
                    , Sequence.send wrappers state <| DbPut key urlValue
                    ]


initialLoadDataState : StorageState model msg
initialLoadDataState =
    makeLoadDataState (\_ _ model -> ( model, Cmd.none )) []


makeLoadDataState : (List ( String, String ) -> List ( String, Meme ) -> model -> ( model, Cmd msg )) -> List KeyPair -> StorageState model msg
makeLoadDataState receiver hashes =
    LoadDataState
        { hashes = hashes
        , names = []
        , images = []
        , memes = []
        , receiver = receiver
        }


startLoadData : Wrappers model msg -> (List ( String, String ) -> List ( String, Meme ) -> model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
startLoadData =
    startLoadDataForImages Nothing


cdr : List a -> List a
cdr list =
    List.tail list |> Maybe.withDefault []


startLoadDataForImages : Maybe (List String) -> Wrappers model msg -> (List ( String, String ) -> List ( String, Meme ) -> model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
startLoadDataForImages maybeImages wrappers receiver model =
    let
        listImageurlsPair =
            { prefix = pK.imageurls
            , subkey = "."
            }

        hashes =
            case maybeImages of
                Just list ->
                    List.map (KeyPair pK.imageurls) list

                Nothing ->
                    []

        state =
            makeLoadDataState receiver (cdr hashes)

        ( mdl, loadDataState ) =
            Sequence.setState wrappers LoadData state model
    in
    ( mdl
    , if maybeImages == Nothing then
        Sequence.send wrappers loadDataState (DbListKeys listImageurlsPair)

      else
        case hashes of
            [] ->
                Cmd.none

            key :: _ ->
                Sequence.send wrappers loadDataState (DbGet key)
    )


loadDataStateProcess : Wrappers model msg -> DbResponse -> StorageState model msg -> ( DbRequest msg, StorageState model msg )
loadDataStateProcess wrappers response storageState =
    case storageState of
        LoadDataState state ->
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
                                        , DbGet key
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
                                -- Can't happen, but no way to continue
                                doneTriplet

                        DbGot { prefix, subkey } value ->
                            if prefix == pK.imageurls then
                                let
                                    state3 =
                                        case
                                            Sequence.decodeExpectedDbGot JD.string
                                                ""
                                                response
                                        of
                                            Nothing ->
                                                state

                                            Just ( _, Nothing ) ->
                                                state

                                            Just ( _, Just url ) ->
                                                { state
                                                    | images =
                                                        ( subkey, url )
                                                            :: state.images
                                                }

                                    ( state4, req ) =
                                        case state3.hashes of
                                            [] ->
                                                ( state3
                                                , DbListKeys <| KeyPair pK.memes "."
                                                )

                                            key :: rest ->
                                                ( { state3 | hashes = rest }
                                                , DbGet key
                                                )
                                in
                                ( False, state4, req )

                            else if prefix == pK.memes then
                                let
                                    state3 =
                                        case
                                            Sequence.decodeExpectedDbGot
                                                ED.memeDecoder
                                                ""
                                                response
                                        of
                                            Nothing ->
                                                state

                                            Just ( _, Nothing ) ->
                                                state

                                            Just ( _, Just meme ) ->
                                                { state
                                                    | memes =
                                                        if
                                                            List.member meme.image.hash <|
                                                                List.map Tuple.first
                                                                    state.images
                                                        then
                                                            ( subkey, meme )
                                                                :: state.memes

                                                        else
                                                            state.memes
                                                }
                                in
                                case state3.names of
                                    [] ->
                                        ( True, state3, DbNothing )

                                    key :: rest ->
                                        ( False
                                        , { state3
                                            | names = rest
                                          }
                                        , DbGet key
                                        )

                            else
                                doneTriplet

                        _ ->
                            -- Not DbKeys or DbGot, ignore it
                            ( False, state, DbNothing )
            in
            if done then
                ( DbCustomRequest <|
                    Task.perform wrappers.sequenceDone
                        (Task.succeed <| loadDataDone wrappers)
                , LoadDataState state2
                )

            else
                ( request, LoadDataState state2 )

        _ ->
            -- Not LoadDataState, ignore it
            ( DbNothing, storageState )


loadDataDone : Wrappers model msg -> model -> ( model, Cmd msg )
loadDataDone wrappers model =
    case Sequence.getState wrappers LoadData model of
        Nothing ->
            ( model, Cmd.none )

        Just state ->
            case state of
                LoadDataState loadDataState ->
                    let
                        ( mdl, cmd ) =
                            loadDataState.receiver loadDataState.images
                                loadDataState.memes
                                model

                        mdl2 =
                            Sequence.setStateOnly wrappers
                                LoadData
                                initialLoadDataState
                                mdl
                    in
                    mdl2 |> withCmd cmd

                _ ->
                    model |> withNoCmd
