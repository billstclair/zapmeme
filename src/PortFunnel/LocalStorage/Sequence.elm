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
    ( DbRequest(..)
    , DbResponse(..)
    , KeyPair
    , State
    , dbResponseToValue
    , decodeExpectedDbGot
    , decodePair
    , encodePair
    , inject
    , injectTask
    , multiProcess
    , process
    , send
    )

{-| Make it easier to create complex state machines from `LocalStorage` contents.

This is too complicated for an example here, but see `ZapMeme.Sequencer` (needs link) for a real-life example.

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


type alias State state msg =
    { state : state
    , label : String
    , process : DbResponse -> state -> ( DbRequest msg, state )
    , sender : LocalStorage.Message -> Cmd msg
    }


multiProcess : LocalStorage.Response -> List ( State state msg, setter ) -> Maybe ( State state msg, setter, Cmd msg )
multiProcess response states =
    let
        pairs =
            List.map (\( state, setter ) -> ( process response state, setter )) states
                |> List.filter (\( pair, _ ) -> pair /= Nothing)
    in
    case pairs of
        [ ( Just ( state, cmd ), setter ) ] ->
            Just ( state, setter, cmd )

        _ ->
            Nothing


process : LocalStorage.Response -> State state msg -> Maybe ( State state msg, Cmd msg )
process response state =
    let
        ( label, responseThunk ) =
            responseToDbResponse response
    in
    if label /= state.label then
        Nothing

    else
        let
            dbResponse =
                Debug.log "process" <| responseThunk ()

            ( request, state2 ) =
                state.process dbResponse state.state
        in
        Just
            ( if state2 == state.state then
                state

              else
                { state | state = state2 }
            , case request of
                DbCustomRequest cmd ->
                    cmd

                DbBatch requests ->
                    List.map (send state) (flattenBatchList requests)
                        |> Cmd.batch

                _ ->
                    send state request
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


send : State state msg -> DbRequest msg -> Cmd msg
send state request =
    case requestToMessage state.label request of
        Just req ->
            state.sender <| Debug.log "send" req

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
injectTask : Injector msg -> State state msg -> DbResponse -> Task Never Value
injectTask injector state response =
    dbResponseToValue injector.prefix state.label response
        |> Task.succeed


type alias Injector msg =
    { prefix : String
    , tagger : Value -> msg
    }


{-| Call `injectTask`, and use `Task.perform` to turn that `Task` into a `Cmd`.

The `(Value -> msg)` function will usually be the `Msg` that receives subscription input from your LocalStorage port.

-}
inject : Injector msg -> State state msg -> DbResponse -> Cmd msg
inject injector state response =
    injectTask injector state response
        |> Task.perform injector.tagger



---
--- Support for tables of state machines.
---


{-| Communication between `Sequence` and your main `Model` and `Msg`.
-}
type alias Wrappers key state model msg =
    { sender : LocalStorage.Message -> Cmd msg
    , injector : { prefix : String, tagger : Value -> msg }
    , localStorageStates : model -> LocalStorageStates key state model msg
    , setLocalStorageStates : LocalStorageStates key state model msg -> model -> model
    , sequenceDone : (model -> ( model, Cmd msg )) -> msg
    }


type LocalStorageStates key state model msg
    = LocalStorageStates
        { label : String
        , wrappers : Wrappers key state model msg
        , table : Dict key (State state msg)
        }


makeLocalStorageStates : String -> Wrappers key state model msg -> List ( key, State state msg ) -> LocalStorageStates key state model msg
makeLocalStorageStates label wrappers states =
    LocalStorageStates
        { label = label
        , wrappers = wrappers
        , table = AssocList.fromList states
        }


getWrappers : LocalStorageStates key state model msg -> Wrappers key state model msg
getWrappers (LocalStorageStates states) =
    states.wrappers


getStates : LocalStorageStates key state model msg -> List ( key, State state msg )
getStates (LocalStorageStates states) =
    AssocList.toList states.table


setStates : List ( key, State state msg ) -> LocalStorageStates key state model msg -> LocalStorageStates key state model msg
setStates stateList (LocalStorageStates states) =
    LocalStorageStates
        { states | table = AssocList.fromList stateList }


getState : key -> LocalStorageStates key state model msg -> Maybe (State state msg)
getState key (LocalStorageStates states) =
    AssocList.get key states.table


putState : key -> State state msg -> LocalStorageStates key state model msg -> LocalStorageStates key state model msg
putState key state (LocalStorageStates states) =
    LocalStorageStates
        { states | table = AssocList.insert key state states.table }


removeState : key -> LocalStorageStates key state model msg -> LocalStorageStates key state model msg
removeState key (LocalStorageStates states) =
    LocalStorageStates
        { states | table = AssocList.remove key states.table }


processStates : LocalStorage.Response -> LocalStorageStates key state model msg -> ( LocalStorageStates key state model msg, Cmd msg )
processStates response (LocalStorageStates states) =
    let
        loop key state ( states3, cmd2 ) =
            case process response state of
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
