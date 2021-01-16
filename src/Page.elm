module Page exposing (Details, view)

import Browser
import Css exposing (pct)
import Html.Styled
    exposing
        ( Attribute
        , Html
        , div
        , main_
        , map
        , toUnstyled
        )
import Html.Styled.Attributes
    exposing
        ( css
        , id
        )



-- PAGE


type alias Details msg =
    { title : String
    , attrs : List (Attribute msg)
    , body : List (Html msg)
    }



-- VIEW


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title = details.title
    , body = [ viewApp toMsg details.attrs details.body |> toUnstyled ]
    }


viewApp : (a -> msg) -> List (Attribute a) -> List (Html a) -> Html msg
viewApp toMsg attrs body =
    div
        [ id "top"
        , css
            [ Css.height (pct 100)
            ]
        ]
        [ map toMsg (viewBody attrs body)
        ]


viewBody : List (Attribute msg) -> List (Html msg) -> Html msg
viewBody attrs body =
    main_
        attrs
        body
