----------------------------------------------------------------------
--
-- Sequence.elm
-- Sequencing a series of LocalStorage operations using a state machine.
-- Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module PortFunnel.LocalStorage.Sequence exposing
    ( Wrappers, LocalStorageStates, State
    , KeyPair, DbRequest(..), DbResponse(..)
    , makeLocalStorageStates, makeNullState
    , update, processStates, process
    , send, inject, injectTask
    , getState, setState, getFullState, setStateOnly
    , dbResponseToValue, decodeExpectedDbGot, decodePair, encodePair
    )

{-| Make it easier to create complex state machines from `LocalStorage` contents.

This is too complicated for an example here, but see [ZapMeme.Sequencer](https://github.com/billstclair/zapmeme/blob/master/src/ZapMeme/Sequencer.elm) for a real-life example.


# Types

@docs Wrappers, LocalStorageStates, State
@docs KeyPair, DbRequest, DbResponse


# Constructors

@docs makeLocalStorageStates, makeNullState


# Processing incoming messages

You will usually use only `update`, but the others let you do lower-level processing yourself.

@docs update, processStates, process


# LocalStorage interaction outside of `State.process`

@docs send, inject, injectTask


# State accessors

@docs getState, setState, getFullState, setStateOnly


# Utilities

@docs dbResponseToValue, decodeExpectedDbGot, decodePair, encodePair

-}

import AssocList exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import PortFunnel
import PortFunnel.LocalStorage as LocalStorage
import Task exposing (Task)


type alias KeyPair =
    { prefix : String
    , subkey : String
    }


type DbRequest msg
    = DbNothing
    | DbGet KeyPair
    | DbPut KeyPair Value
    | DbRemove KeyPair
    | DbListKeys KeyPair
    | DbClear KeyPair
    | DbCustomRequest (Cmd msg)
    | DbBatch (List (DbRequest msg))


type DbResponse
    = DbNoResponse
    | DbGot KeyPair (Maybe Value)
    | DbKeys KeyPair (List KeyPair)


type alias State key state model msg =
    { state : state
    , label : String
    , process : Wrappers key state model msg -> DbResponse -> state -> ( DbRequest msg, state )
    }


type NullState key state model msg
    = NullState (State key state model msg)


dummyProcess : Wrappers key state model msg -> DbResponse -> state -> ( DbRequest msg, state )
dummyProcess wrappers response state =
    ( DbNothing, state )


nullLabel : String
nullLabel =
    "If somebody uses this lable they'll get what they deserve!!"


isNullState : State key state model msg -> Bool
isNullState state =
    nullLabel == state.label


makeNullState : state -> NullState key state model msg
makeNullState state =
    NullState
        { state = state
        , label = nullLabel
        , process = dummyProcess
        }


process : Wrappers key state model msg -> LocalStorage.Response -> State key state model msg -> Maybe ( State key state model msg, Cmd msg )
process wrappers response state =
    let
        ( label, responseThunk ) =
            responseToDbResponse response
    in
    if label /= state.label then
        Nothing

    else
        let
            dbResponse =
                responseThunk ()

            ( request, state2 ) =
                state.process wrappers dbResponse state.state
        in
        Just
            ( { state | state = state2 }
            , case request of
                DbCustomRequest cmd ->
                    cmd

                DbBatch requests ->
                    List.map (send wrappers state) (flattenBatchList requests)
                        |> Cmd.batch

                _ ->
                    send wrappers state request
            )


flattenBatchList : List (DbRequest msg) -> List (DbRequest msg)
flattenBatchList requests =
    let
        loop tail res =
            case tail of
                [] ->
                    List.reverse res

                req :: rest ->
                    case req of
                        DbBatch reqs ->
                            loop rest <|
                                List.concat
                                    [ List.reverse <| flattenBatchList reqs
                                    , res
                                    ]

                        _ ->
                            loop rest <| req :: res
    in
    loop requests []


send : Wrappers key state model msg -> State key state model msg -> DbRequest msg -> Cmd msg
send wrappers state request =
    if isNullState state then
        Cmd.none

    else
        case requestToMessage state.label request of
            Just req ->
                wrappers.sender req

            Nothing ->
                Cmd.none


decodeExpectedDbGot : Decoder value -> String -> DbResponse -> Maybe ( KeyPair, Maybe value )
decodeExpectedDbGot decoder expectedSubkey response =
    case response of
        DbGot pair value ->
            if
                (expectedSubkey /= "")
                    && (pair.subkey /= expectedSubkey)
            then
                Just ( pair, Nothing )

            else
                case value of
                    Nothing ->
                        Just ( pair, Nothing )

                    Just v ->
                        case JD.decodeValue decoder v of
                            Err _ ->
                                Just ( pair, Nothing )

                            Ok result ->
                                Just ( pair, Just result )

        _ ->
            Nothing


responseToDbResponse : LocalStorage.Response -> ( String, () -> DbResponse )
responseToDbResponse response =
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            ( Maybe.withDefault "" label
            , \() -> DbGot (decodePair key) value
            )

        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            ( Maybe.withDefault "" label
            , \() -> DbKeys (decodePair prefix) <| List.map decodePair keys
            )

        _ ->
            ( "", \() -> DbNoResponse )


requestToMessage : String -> DbRequest msg -> Maybe LocalStorage.Message
requestToMessage label request =
    case request of
        DbNothing ->
            Nothing

        DbGet pair ->
            Just <| LocalStorage.getLabeled label (encodePair pair)

        DbPut pair value ->
            Just <| LocalStorage.put (encodePair pair) (Just value)

        DbRemove pair ->
            Just <| LocalStorage.put (encodePair pair) Nothing

        DbListKeys pair ->
            Just <| LocalStorage.listKeysLabeled label (encodePair pair)

        DbClear pair ->
            Just <| LocalStorage.clear label

        _ ->
            Nothing


encodePair : KeyPair -> String
encodePair { prefix, subkey } =
    if prefix == "" then
        subkey

    else
        let
            suffix =
                if subkey == "" then
                    ""

                else if subkey == "." then
                    "."

                else
                    "." ++ subkey
        in
        prefix ++ suffix


decodePair : String -> KeyPair
decodePair key =
    case String.split "." key of
        [] ->
            KeyPair "" ""

        [ prefix ] ->
            KeyPair prefix ""

        [ prefix, "" ] ->
            KeyPair prefix "."

        prefix :: tail ->
            KeyPair prefix (String.join "." tail)


{-| Turn a DbResponse into the Value that would create it,

if received from the LocalStorage port code, in response to a `DbGet` or `DbListKeys` request.

`NoResponse` is treated as `DbGot` with empty string components in its `KeyPair` and `Nothing` as its value. You will likely never want to do that.

-}
dbResponseToValue : String -> String -> DbResponse -> Value
dbResponseToValue prefix label response =
    let
        wrap tag args =
            { moduleName = LocalStorage.moduleName
            , tag = tag
            , args = args
            }
                |> PortFunnel.encodeGenericMessage

        prefixed key =
            encodePair <| KeyPair prefix key
    in
    case response of
        DbGot pair value ->
            wrap "got" <|
                JE.object <|
                    [ ( "label", JE.string label )
                    , ( "key", JE.string (prefixed <| encodePair pair) )
                    , ( "value"
                      , case value of
                            Nothing ->
                                JE.null

                            Just v ->
                                v
                      )
                    ]

        DbKeys pair keys ->
            wrap "keys" <|
                JE.object
                    [ ( "label", JE.string label )
                    , ( "prefix", JE.string (prefixed <| encodePair pair) )
                    , ( "keys"
                      , List.map encodePair keys
                            |> JE.list JE.string
                      )
                    ]

        DbNoResponse ->
            dbResponseToValue prefix label <| DbGot (KeyPair "" "") Nothing


{-| If you receive something outside of a LocalStorage return,

and want to get it back into your state machine, use this.

It returns a `Task` that, if you send it with your LocalStorage `sub` port message, will make it seem as if the given DbResponse was received from LocalStorage.

It's a trivial task wrapper on the result of dbResponseToValue, using `label` property of the `State`.

-}
injectTask : Wrappers key state model msg -> State key state model msg -> DbResponse -> Task Never Value
injectTask wrappers state response =
    dbResponseToValue wrappers.injector.prefix state.label response
        |> Task.succeed


{-| Call `injectTask`, and use `Task.perform` to turn that `Task` into a `Cmd`.

The `(Value -> msg)` function will usually be the `Msg` that receives subscription input from your LocalStorage port.

-}
inject : Wrappers key state model msg -> State key state model msg -> DbResponse -> Cmd msg
inject wrappers state response =
    injectTask wrappers state response
        |> Task.perform wrappers.injector.tagger



---
--- Support for tables of state machines.
---


type alias Injector msg =
    { prefix : String
    , tagger : Value -> msg
    }


{-| Communication between `Sequence` and your main `Model` and `Msg`.
-}
type alias Wrappers key state model msg =
    { sender : LocalStorage.Message -> Cmd msg
    , injector : Injector msg
    , localStorageStates : model -> LocalStorageStates key state model msg
    , setLocalStorageStates : LocalStorageStates key state model msg -> model -> model
    , sequenceDone : (model -> ( model, Cmd msg )) -> msg
    , nullState : NullState key state model msg
    }


type LocalStorageStates key state model msg
    = LocalStorageStates
        { wrappers : Wrappers key state model msg
        , table : Dict key (State key state model msg)
        }


makeLocalStorageStates : Wrappers key state model msg -> List ( key, State key state model msg ) -> LocalStorageStates key state model msg
makeLocalStorageStates wrappers states =
    LocalStorageStates
        { wrappers = wrappers
        , table = AssocList.fromList states
        }


getWrappers : LocalStorageStates key state model msg -> Wrappers key state model msg
getWrappers (LocalStorageStates states) =
    states.wrappers


getFullState : Wrappers key state model msg -> key -> model -> Maybe (State key state model msg)
getFullState wrappers key model =
    let
        (LocalStorageStates states) =
            wrappers.localStorageStates model
    in
    AssocList.get key states.table


getState : Wrappers key state model msg -> key -> model -> Maybe state
getState wrappers key model =
    case getFullState wrappers key model of
        Nothing ->
            Nothing

        Just state ->
            Just state.state


nullState : Wrappers key state model msg -> State key state model msg
nullState wrappers =
    let
        (NullState state) =
            wrappers.nullState
    in
    state


setStateOnly : Wrappers key state model msg -> key -> state -> model -> model
setStateOnly wrappers key stateState model =
    let
        ( res, _ ) =
            setState wrappers key stateState model
    in
    res


setState : Wrappers key state model msg -> key -> state -> model -> ( model, State key state model msg )
setState wrappers key stateState model =
    let
        (LocalStorageStates states) =
            wrappers.localStorageStates model
    in
    case AssocList.get key states.table of
        Nothing ->
            ( model, nullState wrappers )

        Just state ->
            ( wrappers.setLocalStorageStates
                (LocalStorageStates
                    { states
                        | table =
                            AssocList.insert key
                                { state | state = stateState }
                                states.table
                    }
                )
                model
            , state
            )


processStates : LocalStorage.Response -> LocalStorageStates key state model msg -> ( LocalStorageStates key state model msg, Cmd msg )
processStates response (LocalStorageStates states) =
    let
        loop key state ( states3, cmd2 ) =
            case process states.wrappers response state of
                Nothing ->
                    ( states3, cmd2 )

                Just ( state2, cmd3 ) ->
                    ( { states3
                        | table =
                            AssocList.insert key state2 states3.table
                      }
                    , Cmd.batch [ cmd3, cmd2 ]
                    )

        ( states2, cmd ) =
            AssocList.foldl loop ( states, Cmd.none ) states.table
    in
    ( LocalStorageStates states2, cmd )


update : Wrappers key state model msg -> model -> LocalStorage.Response -> ( model, Cmd msg )
update wrappers model response =
    let
        ( states2, cmd ) =
            processStates response (wrappers.localStorageStates model)
    in
    ( wrappers.setLocalStorageStates states2 model
    , cmd
    )
