module Game.Players exposing (Player(..), Players, all, filter, fromMap, get, left, partner, right, set)

import Html exposing (a)
import List


type Player
    = North
    | South
    | East
    | West


partner : Player -> Player
partner player =
    case player of
        North ->
            South

        South ->
            North

        East ->
            West

        West ->
            East


left : Player -> Player
left player =
    case player of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


right : Player -> Player
right player =
    case player of
        North ->
            West

        East ->
            North

        South ->
            East

        West ->
            South


type alias PlayerMap a =
    { north : a
    , south : a
    , east : a
    , west : a
    }


type Players a
    = Players (PlayerMap a)


all : a -> Players a
all val =
    Players
        { north = val
        , south = val
        , east = val
        , west = val
        }


fromMap : PlayerMap a -> Players a
fromMap map =
    Players map


get : Player -> Players a -> a
get player (Players map) =
    case player of
        North ->
            map.north

        South ->
            map.south

        East ->
            map.east

        West ->
            map.west


set : Player -> a -> Players a -> Players a
set player newVal (Players map) =
    case player of
        North ->
            Players { map | north = newVal }

        South ->
            Players { map | south = newVal }

        East ->
            Players { map | east = newVal }

        West ->
            Players { map | west = newVal }


filter : (a -> Bool) -> Players a -> List Player
filter predicate players =
    List.filterMap
        (\player ->
            if predicate (get player players) then
                Just player

            else
                Nothing
        )
        [ North, South, East, West ]
