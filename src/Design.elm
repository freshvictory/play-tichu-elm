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


shadow =
    { lowest = Css.boxShadow4 (px 1) (px 1) (px 3) color.darkestGray
    , low = Css.boxShadow4 (px 3) (px 3) (px 6) color.darkestGray
    , middle = Css.boxShadow4 (px 5) (px 5) (px 15) color.darkestGray
    , high = Css.boxShadow4 (px 10) (px 10) (px 24) color.darkestGray
    , highest = Css.boxShadow4 (px 15) (px 15) (px 35) color.darkestGray
    , underscore = Css.boxShadow4 Css.zero (px 2) (px 6) color.darkestGray
    , focus = Css.boxShadow5 Css.zero Css.zero (px 2) (px 3) color.focus
    , hover = Css.boxShadow5 Css.zero Css.zero Css.zero (px 3) color.primary
    }


focus : Css.Style
focus =
    Css.focus
        [ Css.outline Css.none
        , shadow.focus
        ]


color =
    { black = hsl 43 0.21 0.067
    , lightBlack = hsl 138 0.16 0.133
    , lightestBlack = hsl 135 0.15 0.267
    , darkestGray = hsl 135 0.13 0.4
    , darkGray = hsl 135 0.12 0.533
    , gray = hsl 135 0.12 0.667
    , lightGray = hsl 135 0.12 0.733
    , lightestGray = hsl 135 0.12 0.8
    , offWhite = hsl 135 0.16 0.867
    , white = hsl 135 0.21 0.98
    , darkestPrimary = hsl 38 1 0.48
    , darkPrimary = hsl 41 1 0.58
    , primary = hsl 44 1 0.68
    , lightPrimary = hsl 47 1 0.78
    , lightestPrimary = hsl 50 1 0.88
    , table = hsl 135 0.5 0.5
    , lightTable = hsl 135 0.5 0.85
    , lightestTable = hsl 135 1 0.94
    , focus = hsl 194 0.94 0.25
    }


button =
    let
        baseButton =
            \text msg attributes ->
                H.button
                    (attributes
                        ++ [ E.onClick msg
                           , css
                                [ Css.fontSize font.regular
                                , Css.fontVariant Css.smallCaps
                                , Css.borderRadius borderRadius.inner
                                , Css.border3 (px 2) Css.solid color.lightBlack
                                , Css.padding spacing.xsmall
                                , Css.whiteSpace Css.noWrap
                                , shadow.lowest
                                , focus
                                , Css.Transitions.transition [ Css.Transitions.backgroundColor 100 ]
                                , Css.disabled
                                    [ Css.opacity (Css.num 0.7)
                                    , Css.pointerEvents Css.none
                                    ]
                                ]
                           ]
                    )
                    [ H.text text ]
    in
    { primary =
        \text msg attributes ->
            baseButton
                text
                msg
                (css
                    [ Css.backgroundColor color.primary
                    , Css.color color.black
                    , Css.hover
                        [ Css.backgroundColor color.lightPrimary
                        ]
                    , Css.active
                        [ Css.backgroundColor color.lightestPrimary
                        ]
                    ]
                    :: attributes
                )
    , secondary =
        \text msg attributes ->
            baseButton
                text
                msg
                (css
                    [ Css.backgroundColor color.lightestGray
                    , Css.color color.black
                    , Css.hover
                        [ Css.backgroundColor color.offWhite
                        ]
                    , Css.active
                        [ Css.backgroundColor color.white
                        ]
                    ]
                    :: attributes
                )
    , custom =
        \text msg attributes ->
            baseButton text msg attributes
    }
