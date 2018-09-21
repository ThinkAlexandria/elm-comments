module Comments.Css exposing (CssConfig, defaultCssConfig, exampleStyleSheet, styleCommentEditor)

{-|

@docs CssConfig, defaultCssConfig, exampleStyleSheet, styleCommentEditor

-}

import Css exposing (..)
import Css.Elements exposing (..)


{-| Css classes for use in example
-}
type CssClasses
    = CommentEditor
    | CommentEditorHeader
    | CommentEditorToolbarButton
    | CommentEditorBody
    | MarkdownBody
    | CommentTextInput
    | CommentEditorFooter
    | CommentEditorButton
    | CommentSpacer
    | HorizontalTabNav
    | HorizontalTab
    | HorizontalTabCommentEditorSelected


{-| -}
type alias CssConfig cssClasses =
    { editor : cssClasses
    , header : cssClasses
    , horizontalTabNav : cssClasses
    , horizontalTab : cssClasses
    , horizontalTabSelected : cssClasses
    , toolbarButton : cssClasses
    , body : cssClasses
    , markdownBody : cssClasses
    , textInput : cssClasses
    , footer : cssClasses
    , button : cssClasses
    , spacer : cssClasses
    , toString : cssClasses -> String
    }


{-| -}
defaultCssConfig : CssConfig CssClasses
defaultCssConfig =
    { editor = CommentEditor
    , header = CommentEditorHeader
    , horizontalTabNav = HorizontalTabNav
    , horizontalTab = HorizontalTab
    , horizontalTabSelected = HorizontalTabCommentEditorSelected
    , toolbarButton = CommentEditorToolbarButton
    , body = CommentEditorBody
    , markdownBody = MarkdownBody
    , textInput = CommentTextInput
    , footer = CommentEditorFooter
    , button = CommentEditorButton
    , spacer = CommentSpacer
    , toString =
        \c ->
            case c of
                CommentEditor ->
                    "CommentEditor"

                CommentEditorHeader ->
                    "CommentEditorHeader"

                CommentEditorToolbarButton ->
                    "CommentEditorToolbarButton"

                CommentEditorBody ->
                    "CommentEditorBody"

                MarkdownBody ->
                    "MarkdownBody"

                CommentTextInput ->
                    "CommentTextInput"

                CommentEditorFooter ->
                    "CommentEditorFooter"

                CommentEditorButton ->
                    "CommentEditorButton"

                CommentSpacer ->
                    "CommentSpacer"

                HorizontalTabNav ->
                    "HorizontalTabNav"

                HorizontalTab ->
                    "HorizontalTab"

                HorizontalTabCommentEditorSelected ->
                    "HorizontalTabCommentEditorSelected"
    }


type alias StyleConfig =
    { commonRadius : Float
    , commonBorderThickness : Float
    , borderColor : Css.Color
    , editorBackground : Css.Color
    , buttonHoverColor : Css.Color
    }


defaultStyleConfig : StyleConfig
defaultStyleConfig =
    { commonRadius = 7
    , commonBorderThickness = 3
    , borderColor = hex "#bbb"
    , editorBackground = hex "#fff"
    , buttonHoverColor = hex "#eee"
    }


{-| -}
styleCommentEditor : StyleConfig -> CssConfig cssClasses -> List Snippet
styleCommentEditor styleConfig cssConfig =
    let
        class =
            cssConfig.toString >> Css.class
    in
    [ class cssConfig.editor
        [ border3 (px styleConfig.commonBorderThickness) solid styleConfig.borderColor
        , backgroundColor styleConfig.editorBackground
        , margin (px 0)
        , borderRadius (px styleConfig.commonRadius)

        -- box-shadow: inset 0 1px 1px rgba(0,0,0,0.075),0 0 8px rgba(58,171,240,0.6);
        ]
    , class cssConfig.button
        [ marginLeft (px 5)
        , backgroundColor styleConfig.editorBackground
        , border3 (px styleConfig.commonBorderThickness) solid styleConfig.borderColor
        , padding2 (px 8) (px 12)

        --, shadowLevelTwo
        , borderRadius (px styleConfig.commonRadius)
        , hover
            [ backgroundColor styleConfig.buttonHoverColor
            ]
        ]
    , class cssConfig.header
        [ padding3 (px 6) (px 10) (px 0)
        , displayFlex
        , justifyContent spaceBetween
        , alignItems center
        ]
    , class cssConfig.horizontalTabSelected
        [ borderColor styleConfig.borderColor
        , backgroundColor styleConfig.editorBackground
        , borderBottom (px 0)
        ]
    , class cssConfig.toolbarButton
        [ margin (px 5)
        , padding (px 0)
        , border (px 0)
        , backgroundColor (hex "#fff")
        ]
    , class cssConfig.body
        [ margin (px 10)
        , marginTop (px 0)
        , borderTop3 (px styleConfig.commonBorderThickness) solid styleConfig.borderColor
        , borderBottom3 (px styleConfig.commonBorderThickness) solid styleConfig.borderColor
        ]
    , class cssConfig.markdownBody
        [ borderTop3 (px styleConfig.commonBorderThickness) solid styleConfig.borderColor

        {- Match the margin of CommentInput text but reduce the padding so
           the rendered text looks a little bit bigger than the draft inside
           the text area, a little perceptual cue
        -}
        , margin2 (px 0) (px 10)
        , padding2 (px 10) (px 5)
        , children
            [ selector ":last-child"
                [ marginBottom (px 0) ]
            , selector ":first-child"
                [ marginTop (px 0) ]
            , Css.Elements.pre
                [ backgroundColor (hex "#272822")
                , color (hex "#ddd")
                , padding (px 10)
                , borderRadius (px 5)
                ]
            ]
        ]
    , class cssConfig.textInput
        [ width (pct 100)
        , padding (px 10)
        , minHeight (px 100)
        , maxHeight (px 500)
        , border (px 0)
        , resize vertical
        , boxSizing borderBox
        ]

    {- Pushes the helptext to left and action buttons to the right -}
    , class cssConfig.footer
        [ displayFlex
        , justifyContent spaceBetween
        , margin (px 10)
        ]

    {- modify the alignment of the Markdown icon -}
    , Css.class "octiconMarkdown"
        [ verticalAlign middle ]
    , class cssConfig.spacer
        [ height (px 30)
        , marginLeft (px 30)
        , borderLeft3 (px 3) solid styleConfig.borderColor
        ]
    ]



-- Derived from Material Design Lite .mdl-shadow--2dp


shadowLevelTwo : Css.Style
shadowLevelTwo =
    property "box-shadow" "0 2px 2px 0 rgba(0,0,0,.14),0 3px 1px -2px rgba(0,0,0,.2),0 1px 5px 0 rgba(0,0,0,.12)"



{- Supplamental styles for stand alone examples. It is expected that you will
   use your own horizontal tab nav implementation
-}


styleHorizontalTabNav : StyleConfig -> CssConfig cssClasses -> List Snippet
styleHorizontalTabNav styleConfig cssConfig =
    {- The border bottom of this element should overlap the border bottom of
       the CommentEditorHeader so that the background of the HorizontalTabSelected
       tab appears to merge with the comment area background below
    -}
    let
        class =
            cssConfig.toString >> Css.class
    in
    [ class cssConfig.horizontalTabNav
        [ marginBottom (px (2 * -styleConfig.commonBorderThickness))
        , borderColor inherit
        ]

    {- Each different component will need to implement a selected class that
       matches the background color of the containing widget
    -}
    , class cssConfig.horizontalTab
        [ border3 (px styleConfig.commonBorderThickness) solid transparent
        , backgroundColor transparent
        , padding2 (px 8) (px 12)
        , borderTopLeftRadius (px styleConfig.commonRadius)
        , borderTopRightRadius (px styleConfig.commonRadius)

        {- Prevent the tab from getting too small when there are only 3
           letters such as "All"
        -}
        , minWidth (Css.em 4)
        , color inherit
        ]
    ]


{-| -}
exampleStyleSheet : Css.Stylesheet
exampleStyleSheet =
    stylesheet <|
        List.concat
            [ styleHorizontalTabNav defaultStyleConfig defaultCssConfig
            , styleCommentEditor defaultStyleConfig defaultCssConfig
            ]
