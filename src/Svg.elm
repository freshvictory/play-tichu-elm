module Svg exposing (..)

import Css
import Svg.Styled as Svg exposing (Svg, svg)
import Svg.Styled.Attributes as A


primaryColor : String -> Css.Style
primaryColor color =
    Css.property "--c-svg-primary" color


secondaryColor : String -> Css.Style
secondaryColor color =
    Css.property "--c-svg-secondary" color


card : ( Float, Float, Float ) -> Svg msg
card ( x, y, rotate ) =
    Svg.rect
        [ A.x "0"
        , A.y "0"
        , A.width "30"
        , A.height "45"
        , A.rx "3"
        , A.css
            [ Css.property "transform-origin" "15px 15px"
            , Css.transforms
                [ Css.translateX (Css.px x)
                , Css.translateY (Css.px y)
                , Css.rotateZ (Css.deg rotate)
                ]
            ]
        , A.fill "var(--c-svg-primary, #fff)"
        , A.stroke "var(--c-svg-secondary, #000)"
        , A.strokeMiterlimit "10"
        , A.strokeWidth "2px"
        ]
        []


hand : Svg msg
hand =
    svg
        [ A.viewBox "0 0 60 60"
        ]
        (List.map
            card
            [ ( 4, 7.5, -15 )
            , ( 15, 4, 0 )
            , ( 26, 7.5, 15 )
            ]
        )


stack : Svg msg
stack =
    svg
        [ A.viewBox "0 0 60 60"
        ]
        (List.map
            card
            [ ( 8, 1, 0 )
            , ( 15, 7.5, 0 )
            , ( 22, 14, 0 )
            ]
        )
