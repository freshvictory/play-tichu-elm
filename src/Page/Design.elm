module Page.Design exposing (Model, Msg, init, update, view)

import Css exposing (Color, Style, backgroundColor, border3, boxShadow5, displayFlex, height, inset, margin, padding, property, px, solid, width, zero)
import Design exposing (borderRadius, button, color, shadow, spacing)
import Html.Styled
    exposing
        ( Html
        , div
        , header
        , nav
        , text
        )
import Html.Styled.Attributes exposing (css)
import Page
import Svg
import Svg.Styled exposing (Svg)



-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Page.Details Msg
view model =
    { title = "Design"
    , attrs = []
    , body =
        [ viewHeader
        , viewContent
        ]
    }


viewHeader : Html Msg
viewHeader =
    header
        []
        [ nav
            []
            []
        ]


viewContent : Html Msg
viewContent =
    div
        [ css
            [ backgroundColor color.table
            , padding spacing.large
            , property "display" "grid"
            , property "row-gap" "2rem"
            ]
        ]
        [ viewColors
        , viewButtons
        , viewShadows
        , viewSvgs
        ]


viewColors : Html Msg
viewColors =
    div
        []
        [ div
            [ css
                [ displayFlex
                ]
            ]
            [ viewColor color.black
            , viewColor color.lightBlack
            , viewColor color.lightestBlack
            , viewColor color.darkestGray
            , viewColor color.darkGray
            , viewColor color.gray
            , viewColor color.lightGray
            , viewColor color.lightestGray
            , viewColor color.offWhite
            , viewColor color.white
            ]
        , div
            [ css [ displayFlex ] ]
            [ viewColor color.darkestPrimary
            , viewColor color.darkPrimary
            , viewColor color.primary
            , viewColor color.lightPrimary
            , viewColor color.lightestPrimary
            ]
        ]


viewColor : Color -> Html Msg
viewColor value =
    div
        [ css
            [ width (px 32)
            , height (px 32)
            , backgroundColor value
            , margin spacing.medium
            , boxShadow5 inset zero zero (px 2) color.black
            , Css.borderRadius borderRadius.inner
            ]
        ]
        []


viewButtons : Html Msg
viewButtons =
    div
        [ css
            [ property "display" "grid"
            , property "grid-auto-flow" "column"
            , property "grid-auto-columns" "max-content"
            , property "grid-column-gap" "1rem"
            ]
        ]
        [ button.primary "Primary" NoOp []
        , button.secondary "Secondary" NoOp []
        ]


viewShadows : Html Msg
viewShadows =
    div
        [ css
            [ property "display" "grid"
            , property "grid-auto-flow" "column"
            , property "grid-auto-columns" "max-content"
            , property "grid-column-gap" "1rem"
            ]
        ]
        [ viewShadow "Lowest" shadow.lowest
        , viewShadow "Low" shadow.low
        , viewShadow "Middle" shadow.middle
        , viewShadow "High" shadow.high
        , viewShadow "Highest" shadow.highest
        ]


viewShadow : String -> Style -> Html Msg
viewShadow name shadow =
    div
        [ css
            [ shadow
            , width (px 100)
            , height (px 100)
            , backgroundColor color.lightGray
            ]
        ]
        [ text name ]


viewSvgs : Html Msg
viewSvgs =
    div
        [ css
            [ property "display" "grid"
            , property "grid-auto-flow" "column"
            , property "grid-auto-columns" "max-content"
            , property "grid-column-gap" "1rem"
            ]
        ]
        [ viewSvg Svg.hand
        , viewSvg Svg.stack
        ]


viewSvg : Svg Msg -> Html Msg
viewSvg svg =
    div
        [ css
            [ width (px 60)
            , height (px 60)
            , border3 (px 2) solid color.lightestGray
            ]
        ]
        [ svg ]
