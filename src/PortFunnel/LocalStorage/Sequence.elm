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
    , decodeExpectedDbGot
    , multiProcess
    , process
    , send
    )

import Json.Decode as JD exposing (Decoder, Value)
import PortFunnel.LocalStorage as LocalStorage


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


type DbResponse
    = DbNoResponse
    | DbGot KeyPair (Maybe Value)
    | DbKeys (List KeyPair)


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
            ( request, state2 ) =
                state.process (responseThunk ()) state.state
        in
        Just
            ( if state2 == state.state then
                state

              else
                { state | state = state2 }
            , case request of
                DbCustomRequest cmd ->
                    cmd

                _ ->
                    send request state
            )


send : DbRequest msg -> State state msg -> Cmd msg
send request state =
    case requestToMessage state.label request of
        Just req ->
            state.sender req

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
            Just <| LocalStorage.put (encodePair pair) (Just value)

        DbRemove pair ->
            Just <| LocalStorage.put (encodePair pair) Nothing

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
