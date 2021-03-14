module Design exposing (..)

import Css exposing (hsl, px, rem)
import Css.Transitions
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as E


font : { small : Css.Rem, regular : Css.Rem, large : Css.Rem, xlarge : Css.Rem, smallHeader : Css.Rem, mediumHeader : Css.Rem, largeHeader : Css.Rem, huge : Css.Rem }
font =
    { small = rem 0.875
    , regular = rem 1
    , large = rem 1.125
    , xlarge = rem 1.25
    , smallHeader = rem 1.5
    , mediumHeader = rem 1.875
    , largeHeader = rem 2.25
    , huge = rem 3
    }


spacing : { xxsmall : Css.Rem, xsmall : Css.Rem, small : Css.Rem, medium : Css.Rem, large : Css.Rem, xlarge : Css.Rem, xxlarge : Css.Rem, xxxlarge : Css.Rem }
spacing =
    { xxsmall = rem 0.25
    , xsmall = rem 0.5
    , small = rem 0.75
    , medium = rem 1
    , large = rem 1.5
    , xlarge = rem 2
    , xxlarge = rem 3
    , xxxlarge = rem 4
    }


borderRadius : { inner : Css.Rem, middle : Css.Rem, outer : Css.Rem }
borderRadius =
    { inner = rem 0.25
    , middle = rem 0.75
    , outer = rem 1
    }


shadow : { lowest : Css.Style, low : Css.Style, middle : Css.Style, high : Css.Style, highest : Css.Style, underscore : Css.Style }
shadow =
    { lowest = Css.boxShadow4 (px 1) (px 1) (px 3) color.darkestGray.color
    , low = Css.boxShadow4 (px 3) (px 3) (px 6) color.darkestGray.color
    , middle = Css.boxShadow4 (px 5) (px 5) (px 15) color.darkestGray.color
    , high = Css.boxShadow4 (px 10) (px 10) (px 24) color.darkestGray.color
    , highest = Css.boxShadow4 (px 15) (px 15) (px 35) color.darkestGray.color
    , underscore = Css.boxShadow4 Css.zero (px 2) (px 6) color.darkestGray.color
    }


focus : Css.Style
focus =
    Css.focus
        [ Css.outline Css.none
        , Css.boxShadow5 Css.zero Css.zero (px 2) (px 3) color.focus.color
        ]


colorDefinition : ( Float, Float, Float ) -> { color : Css.Color, string : String }
colorDefinition ( h, s, l ) =
    { color = hsl h s l
    , string =
        "hsl("
            ++ String.fromFloat h
            ++ ", "
            ++ String.fromFloat (s * 100)
            ++ "%, "
            ++ String.fromFloat (l * 100)
            ++ "%)"
    }


color =
    { black = colorDefinition ( 43, 0.21, 0.067 )
    , lightBlack = colorDefinition ( 42, 0.16, 0.133 )
    , lightestBlack = colorDefinition ( 39, 0.15, 0.267 )
    , darkestGray = colorDefinition ( 39, 0.13, 0.4 )
    , darkGray = colorDefinition ( 39, 0.12, 0.533 )
    , gray = colorDefinition ( 39, 0.12, 0.667 )
    , lightGray = colorDefinition ( 39, 0.13, 0.733 )
    , lightestGray = colorDefinition ( 39, 0.15, 0.8 )
    , offWhite = colorDefinition ( 39, 0.16, 0.867 )
    , white = colorDefinition ( 39, 0.21, 0.937 )
    , darkestPrimary = colorDefinition ( 38, 1, 0.48 )
    , darkPrimary = colorDefinition ( 41, 1, 0.58 )
    , primary = colorDefinition ( 44, 1, 0.68 )
    , lightPrimary = colorDefinition ( 47, 1, 0.78 )
    , lightestPrimary = colorDefinition ( 50, 1, 0.88 )
    , table = colorDefinition ( 135, 0.5, 0.5 )
    , focus = colorDefinition ( 194, 0.94, 0.25 )
    }


button : { primary : String -> b -> List Css.Style -> Html b, secondary : String -> a -> List Css.Style -> Html a }
button =
    let
        baseButton =
            \text msg styles ->
                H.button
                    [ E.onClick msg
                    , css
                        ([ Css.fontSize font.regular
                         , Css.fontVariant Css.smallCaps
                         , Css.borderRadius borderRadius.inner
                         , Css.border3 (px 2) Css.solid color.lightBlack.color
                         , Css.padding spacing.xsmall
                         , Css.color color.black.color
                         , shadow.lowest
                         , focus
                         , Css.Transitions.transition [ Css.Transitions.backgroundColor 100 ]
                         ]
                            ++ styles
                        )
                    ]
                    [ H.text text ]
    in
    { primary =
        \text msg styles ->
            baseButton
                text
                msg
                ([ Css.backgroundColor color.primary.color
                 , Css.hover
                    [ Css.backgroundColor color.lightPrimary.color
                    ]
                 , Css.active
                    [ Css.backgroundColor color.lightestPrimary.color
                    ]
                 ]
                    ++ styles
                )
    , secondary =
        \text msg styles ->
            baseButton
                text
                msg
                ([ Css.backgroundColor color.lightestGray.color
                 , Css.hover
                    [ Css.backgroundColor color.offWhite.color
                    ]
                 , Css.active
                    [ Css.backgroundColor color.white.color
                    ]
                 ]
                    ++ styles
                )
    }
