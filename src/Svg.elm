module Svg exposing (..)

import Svg.Styled as Svg exposing (Svg, svg)
import Svg.Styled.Attributes as A


hand : Svg msg
hand =
    svg
        [ A.viewBox "0 0 58.5 54.5"
        ]
        [ Svg.rect
            [ A.x "6.6"
            , A.y "6.78"
            , A.width "27.05"
            , A.height "47.15"
            , A.rx "3"
            , A.transform "translate(-8.41 2.69) rotate(-15.83)"
            , A.fill "currentColor"
            , A.stroke "#000"
            , A.strokeMiterlimit "10"
            , A.strokeWidth "3px"
            ]
            []
        , Svg.rect
            [ A.x "15.55"
            , A.y "2"
            , A.width "27.05"
            , A.height "47.15"
            , A.rx "3"
            , A.fill "currentColor"
            , A.stroke "#000"
            , A.strokeMiterlimit "10"
            , A.strokeWidth "3px"
            ]
            []
        , Svg.rect
            [ A.x "15.39"
            , A.y "18.49"
            , A.width "47.15"
            , A.height "27.05"
            , A.rx "3"
            , A.transform "translate(-5.33 53.73) rotate(-70)"
            , A.fill "currentColor"
            , A.stroke "#000"
            , A.strokeMiterlimit "10"
            , A.strokeWidth "3px"
            ]
            []
        ]


stack : Svg msg
stack =
    svg
        [ A.viewBox "0 0 56 58"
        ]
        [ Svg.rect
            [ A.x "6.5"
            , A.y "1.5"
            , A.width "27"
            , A.height "47"
            , A.rx "3"
            , A.fill "currentColor"
            , A.stroke "#000"
            , A.strokeMiterlimit "10"
            , A.strokeWidth "3px"
            ]
            []
        , Svg.rect
            [ A.x "14.5"
            , A.y "5.5"
            , A.width "27"
            , A.height "47"
            , A.rx "3"
            , A.fill "currentColor"
            , A.stroke "#000"
            , A.strokeMiterlimit "10"
            , A.strokeWidth "3px"
            ]
            []
        , Svg.rect
            [ A.x "22.5"
            , A.y "9.5"
            , A.width "27"
            , A.height "47"
            , A.rx "3"
            , A.fill "currentColor"
            , A.stroke "#000"
            , A.strokeMiterlimit "10"
            , A.strokeWidth "3px"
            ]
            []
        ]
