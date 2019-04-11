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
    , StateRecord
    , makeState
    )

import Json.Decode exposing (Value)
import PortFunnel.LocalStorage as LocalStorage


type alias KeyPair =
    { prefix : String
    , subkey : String
    }


type State state msg
    = State (StateRecord state msg)


type DbRequest msg
    = DbNothing
    | DbGet KeyPair
    | DbPut KeyPair (Maybe Value)
    | DbListKeys KeyPair
    | DbClear KeyPair
    | DbCustomRequest (Cmd msg)


type DbResponse
    = DbNoResponse
    | DbGot KeyPair (Maybe Value)
    | DbKeys (List KeyPair)


type alias StateRecord state msg =
    { state : state
    , label : String
    , process : DbResponse -> state -> ( DbRequest msg, state )
    , sender : LocalStorage.Message -> Cmd msg
    }


makeState : StateRecord state msg -> State state msg
makeState record =
    -- I don't know if there will ever be anything else here.
    State record


getState : State state msg -> state
getState (State { state }) =
    state


setState : state -> State state msg -> State state msg
setState state (State record) =
    State { record | state = state }


getLabel : State state msg -> String
getLabel (State { label }) =
    label


process : LocalStorage.Response -> State state msg -> Maybe ( State state msg, Cmd msg )
process response ((State record) as state) =
    let
        ( label, responseThunk ) =
            responseToDbResponse response
    in
    if label /= record.label then
        Nothing

    else
        let
            ( request, state2 ) =
                record.process (responseThunk ()) record.state
        in
        Just
            ( if record.state == state2 then
                state

              else
                State { record | state = state2 }
            , case request of
                DbCustomRequest cmd ->
                    cmd

                _ ->
                    case requestToMessage record.label request of
                        Just req ->
                            record.sender req

                        Nothing ->
                            Cmd.none
            )


responseToDbResponse : LocalStorage.Response -> ( String, () -> DbResponse )
responseToDbResponse response =
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            ( Maybe.withDefault "" label
            , \() -> DbGot (decodePair key) value
            )

        LocalStorage.ListKeysResponse { label, keys } ->
            ( Maybe.withDefault "" label
            , \() -> DbKeys <| List.map decodePair keys
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
            Just <| LocalStorage.put (encodePair pair) value

        DbListKeys pair ->
            Just <| LocalStorage.listKeysLabeled label (encodePair pair)

        DbClear pair ->
            Just <| LocalStorage.clear label

        DbCustomRequest _ ->
            Nothing


encodePair : KeyPair -> String
encodePair { prefix, subkey } =
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
