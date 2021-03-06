module Game.Cards exposing (ActionResult(..), Card, CardState(..), Cards, Deal, Deck, Location(..), PlayableDeck, PlayableDeckDefinition, PlayedLocation(..), PlayerLocation(..), act, buildDeck, byPlay, findById, selectFrom, shuffle)

import Array exposing (Array)
import Dict exposing (Dict)
import Game.Players exposing (Player(..))
import List
import Random
import Random.Array


type alias Card suit =
    { id : String
    , displayName : String
    , fullName : String
    , rank : Int
    , value : Int
    , suit : suit
    }


type alias Deck suit =
    List (Card suit)


type CardState
    = FaceUp
    | FaceDown


type alias CardInPlay suit =
    { definition : Card suit
    , location : Location
    }


type Location
    = Deck
    | PlayerLocation PlayerLocation Player
    | PlayedLocation PlayedLocation Play


type PlayerLocation
    = Hand
    | Taken
    | InFront CardState
    | PassingTo Player


type PlayedLocation
    = Table


type Cards suit
    = Cards (List (CardInPlay suit)) Plays


type alias Deal =
    List ( Location, Int )


type Play
    = Play Int Player


type Plays
    = Plays Int (Dict Int Player)


findById : String -> Cards suit -> Maybe (CardInPlay suit)
findById id (Cards cards _) =
    List.head
        (List.filter
            (\c -> c.definition.id == id)
            cards
        )


selectFrom : Location -> Cards suit -> List (Card suit)
selectFrom location (Cards cards _) =
    List.filterMap
        (\card ->
            if card.location == location then
                Just card.definition

            else
                Nothing
        )
        cards


byPlay : PlayedLocation -> Cards suit -> List ( Player, List (Card suit) )
byPlay location (Cards cards plays) =
    let
        cardsInLocation =
            List.filterMap
                (\card ->
                    case card.location of
                        PlayedLocation cardLocation (Play playId player) ->
                            if cardLocation == location then
                                Just ( playId, player, card.definition )

                            else
                                Nothing

                        _ ->
                            Nothing
                )
                cards

        playIdToPlays =
            List.foldl
                (\( playId, player, card ) dict ->
                    case Dict.get playId dict of
                        Nothing ->
                            Dict.insert playId ( player, [ card ] ) dict

                        Just ( p, c ) ->
                            Dict.insert playId ( p, c ++ [ card ] ) dict
                )
                Dict.empty
                cardsInLocation
    in
    Dict.values playIdToPlays


emptyPlays : Plays
emptyPlays =
    Plays 0 Dict.empty


addPlay : Player -> Plays -> ( Plays, Play )
addPlay player (Plays nextId dict) =
    ( Plays (nextId + 1) (Dict.insert nextId player dict)
    , Play nextId player
    )


play : Player -> List (Card suit) -> PlayedLocation -> Cards suit -> Cards suit
play player cardsToPlay location (Cards cards plays) =
    let
        ( newPlays, newPlay ) =
            addPlay player plays

        newLocation =
            PlayedLocation location newPlay
    in
    moveInternal cardsToPlay newLocation (Cards cards newPlays)


move : List (Card suit) -> Location -> Cards suit -> Cards suit
move cardsToMove newLocation cards =
    moveInternal cardsToMove newLocation cards


moveInternal : List (Card suit) -> Location -> Cards suit -> Cards suit
moveInternal cardsToMove newLocation (Cards cards plays) =
    Cards
        (List.map
            (\card ->
                if List.member card.definition cardsToMove then
                    { definition = card.definition
                    , location = newLocation
                    }

                else
                    card
            )
            cards
        )
        plays


mapCards : (CardInPlay suit -> Location) -> Cards suit -> Cards suit
mapCards cardToLocation (Cards cards plays) =
    Cards
        (List.map
            (\card ->
                { definition = card.definition
                , location = cardToLocation card
                }
            )
            cards
        )
        plays


type ActionResult suit
    = MoveCards (CardInPlay suit -> Bool) Location
    | MapCards (CardInPlay suit -> Location)
    | PlayCards Player (List (Card suit)) PlayedLocation
    | NoOp


type alias PlayableDeckDefinition suit =
    { deck : Deck suit
    , deal : Deal
    }


type alias PlayableDeck suit =
    { deck : Deck suit
    , deal : Deck suit -> Cards suit
    }


act : ActionResult suit -> Cards suit -> Cards suit
act action (Cards cards plays) =
    case action of
        MoveCards predicate newLocation ->
            move
                (List.map .definition (List.filter predicate cards))
                newLocation
                (Cards cards plays)

        MapCards mapping ->
            mapCards mapping (Cards cards plays)

        PlayCards player cardsToPlay newLocation ->
            play player cardsToPlay newLocation (Cards cards plays)

        NoOp ->
            Cards cards plays


buildDeck : PlayableDeckDefinition suit -> PlayableDeck suit
buildDeck deckDefinition =
    { deck = deckDefinition.deck
    , deal = deal deckDefinition.deal
    }


deal : Deal -> Deck suit -> Cards suit
deal spec deck =
    let
        numberOfCards =
            List.length deck

        locations =
            List.concatMap
                (\( location, count ) ->
                    List.repeat count location
                )
                spec

        deckToDeal =
            List.map2 (\card location -> ( card, location )) deck locations

        deckRemaining =
            List.drop (List.length locations) deck
    in
    Cards
        (List.append
            (List.map
                (\( card, location ) ->
                    { definition = card
                    , location = location
                    }
                )
                deckToDeal
            )
            (List.map
                (\card ->
                    { definition = card
                    , location = Deck
                    }
                )
                deckRemaining
            )
        )
        emptyPlays


shuffle : List a -> Random.Generator (List a)
shuffle list =
    list
        |> Array.fromList
        |> Random.Array.shuffle
        |> Random.andThen (\arr -> Random.constant (Array.toList arr))
