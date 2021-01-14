module Api exposing (..)

import Http
import Json.Decode exposing (Decoder, field)
import Json.Encode



type Game
    = Tichu
    | Gems


type alias Player =
    { id : String
    , name : String
    , hand : List Int
    , secondDeal : List Int
    , passedCards : Bool
    , tricks : List Int
    }


type alias LobbyState =
    { id : String
    , game : Game
    }


type Stage
    = Lobby { id : String }


type alias State =
    { sender : String
    , rewind : Bool
    , state : Stage
    }


baseUrl : String
baseUrl =
    "https://play-tichu-api.azurewebsites.net"


joinGame : (Result Http.Error () -> msg) -> String -> String -> Cmd msg
joinGame m gameId userId =
    Http.post
        { url = baseUrl ++ "/api/game/" ++ gameId ++ "/join?userid=" ++ userId
        , body = Http.emptyBody
        , expect = Http.expectWhatever m
        }


leaveAllGames : (Result Http.Error () -> msg) -> String -> Cmd msg
leaveAllGames m userId =
    Http.post
        { url = baseUrl ++ "/api/reset/" ++ userId
        , body = Http.emptyBody
        , expect = Http.expectWhatever m
        }


pushState : (Result Http.Error () -> msg) -> String -> State -> Cmd msg
pushState m gameId state =
    Http.post
        { url = baseUrl ++ "/api/game/" ++ gameId ++ "/state"

        -- , body = Http.jsonBody (stateEncoder state)
        , body = Http.emptyBody
        , expect = Http.expectWhatever m
        }
