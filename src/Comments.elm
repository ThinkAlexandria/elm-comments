module Comments exposing
    ( Msg(..), State, defaultState, update, viewComment, viewCommentList
    , getEditDraft, getNewDraft, removeEditDraft, removeNewDraft
    )

{-| Standalone github style markdown comments editor

@docs Msg, State, defaultState, update, viewComment, viewCommentList

@docs getEditDraft, getNewDraft, removeEditDraft, removeNewDraft

-}

import Comments.Css exposing (CssConfig)
import Css.Primer.Tooltips.Selectors as Tooltips
import Css.Primer.Tooltips.Selectors.Classes exposing (CssClasses(..))
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, form, h2, input, label, li, nav, p, span, text, textarea, ul)
import Html.Attributes exposing (checked, for, href, placeholder, style, tabindex, type_)
import Html.Events exposing (onClick, onFocus, onInput)
import Html.Keyed
import I18Next exposing (Translations, t)
import Iso8601
import Json.Decode
import Markdown
import Octicons
import Time exposing (Posix)


octiconsDefaultOptions =
    Octicons.defaultOptions



-- rename the translation functions from I18Next


translate : Translations -> String -> String
translate =
    t



-- MODEL


{-| -}
type alias State key =
    { newCommentDrafts : Dict key CommentDraft
    , editCommentDrafts : Dict key (Dict Int CommentDraft)
    }


{-| -}
defaultState : State key
defaultState =
    { newCommentDrafts = Dict.empty
    , editCommentDrafts = Dict.empty
    }


type alias CommentDraft =
    { showMarkdownPreview : Bool
    , markdown : String
    }


defaultCommentDraft : CommentDraft
defaultCommentDraft =
    { showMarkdownPreview = False
    , markdown = ""
    }



{- Dummy type used to keep track of which comment we are editing -}


type CommentDraftSelector key
    = Existing key Int
    | New key


{-| store the comment drafts keyed by task node in the state because each
user will have their own comment drafts.
-}
setDraftMarkdown : String -> CommentDraft -> CommentDraft
setDraftMarkdown x draft =
    { draft | markdown = x }


setDraftPreviewFlag : Bool -> CommentDraft -> CommentDraft
setDraftPreviewFlag flag draft =
    { draft | showMarkdownPreview = flag }



{- Creates a new draft or overwrites the existing value -}
{- Creates a new draft or overwrites the existing value -}


insertEditDraft : comparable -> Int -> String -> State comparable -> State comparable
insertEditDraft key commentIndex commentText state =
    let
        oldRow =
            Maybe.withDefault
                Dict.empty
                (Dict.get key state.editCommentDrafts)

        draft : CommentDraft
        draft =
            { showMarkdownPreview = False
            , markdown = commentText
            }
    in
    { state
        | editCommentDrafts =
            Dict.insert
                key
                (Dict.insert commentIndex draft oldRow)
                state.editCommentDrafts
    }


{-| -}
getEditDraft : comparable -> Int -> State comparable -> Maybe String
getEditDraft key commentIndex state =
    Dict.get key state.editCommentDrafts
        |> Maybe.andThen (Dict.get commentIndex)
        |> Maybe.andThen (\x -> Just x.markdown)


{-| -}
removeEditDraft : comparable -> Int -> State comparable -> State comparable
removeEditDraft key commentIndex state =
    let
        oldRow =
            Maybe.withDefault
                Dict.empty
                (Dict.get key state.editCommentDrafts)
    in
    let
        newRow =
            Dict.remove commentIndex oldRow
    in
    { state
        | editCommentDrafts =
            if Dict.isEmpty newRow then
                Dict.remove key state.editCommentDrafts

            else
                Dict.insert
                    key
                    newRow
                    state.editCommentDrafts
    }


{-| -}
getNewDraft : comparable -> State comparable -> Maybe String
getNewDraft key state =
    Dict.get key state.newCommentDrafts
        |> Maybe.andThen (\x -> Just x.markdown)


{-| -}
removeNewDraft : comparable -> State comparable -> State comparable
removeNewDraft key state =
    { state | newCommentDrafts = Dict.remove key state.newCommentDrafts }



-- UPDATE


{-| -}
type Msg key
    = Internal (InternalMsg key)
      -- The user handles these
    | NewComment key
    | UpdateComment key Int
    | DeleteComment key Int


type InternalMsg key
    = UpdateCommentDraft (CommentDraftSelector key) String
    | CreateCommentDraft (CommentDraftSelector key)
    | DeleteCommentDraft (CommentDraftSelector key)
    | PreviewCommentDraft (CommentDraftSelector key) Bool


{-| I recommend passing in a closure over your datastructure storing the comments
-}
update : (comparable -> Int -> Maybe String) -> InternalMsg comparable -> State comparable -> ( State comparable, Cmd (Msg comparable) )
update createDraftFromExisting msg state =
    case msg of
        DeleteCommentDraft selector ->
            case selector of
                Existing key commentIndex ->
                    ( removeEditDraft key commentIndex state
                    , Cmd.none
                    )

                New key ->
                    ( { state | newCommentDrafts = Dict.remove key state.newCommentDrafts }
                    , Cmd.none
                    )

        CreateCommentDraft selector ->
            case selector of
                Existing key commentIndex ->
                    {- The comment should always exist already, so the withDefault
                       should not be necessary. Right now if the comment is deleted
                       (which means it should not be rendered), and we go to edit it
                       , then upon completing the edit, the deleted comment contents
                       will be updated, but it will remain deleted. TODO: remove this
                       unreachable state space
                    -}
                    case getEditDraft key commentIndex state of
                        Just _ ->
                            ( state
                            , Cmd.none
                            )

                        Nothing ->
                            case createDraftFromExisting key commentIndex of
                                Just commentToEdit ->
                                    ( insertEditDraft key commentIndex commentToEdit state
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( state
                                    , Cmd.none
                                    )

                New key ->
                    if Dict.member key state.newCommentDrafts then
                        ( state
                        , Cmd.none
                        )

                    else
                        ( { state
                            | newCommentDrafts =
                                Dict.insert
                                    key
                                    defaultCommentDraft
                                    state.newCommentDrafts
                          }
                        , Cmd.none
                        )

        UpdateCommentDraft selector value ->
            case selector of
                Existing key commentIndex ->
                    ( { state
                        | editCommentDrafts =
                            Dict.update key
                                (Maybe.map
                                    (Dict.update
                                        commentIndex
                                        (Maybe.map <| setDraftMarkdown value)
                                    )
                                )
                                state.editCommentDrafts
                      }
                    , Cmd.none
                    )

                New key ->
                    ( { state
                        | newCommentDrafts =
                            Dict.update
                                key
                                (Maybe.map <| setDraftMarkdown value)
                                state.newCommentDrafts
                      }
                    , Cmd.none
                    )

        PreviewCommentDraft selector showMarkdownPreview ->
            case selector of
                Existing key commentIndex ->
                    ( { state
                        | editCommentDrafts =
                            Dict.update key
                                (Maybe.map
                                    (Dict.update
                                        commentIndex
                                        (Maybe.map <| setDraftPreviewFlag showMarkdownPreview)
                                    )
                                )
                                state.editCommentDrafts
                      }
                    , Cmd.none
                    )

                New key ->
                    ( { state
                        | newCommentDrafts =
                            Dict.update
                                key
                                (Maybe.map <| setDraftPreviewFlag showMarkdownPreview)
                                state.newCommentDrafts
                      }
                    , Cmd.none
                    )



-- VIEW


type alias Config cssClasses msg key =
    { cssConfig : CssConfig cssClasses
    , translations : Translations
    , toMsg : Msg key -> msg
    }


type alias Comment r m i =
    { r
        | markdown : String
        , metadata :
            { m
                | createdBy : i
                , createdTimestamp : Posix
                , modifiedBy : i
                , modifiedTimestamp : Posix
                , isDeleted : Bool
            }
    }


{-| -}
viewCommentList :
    Config cssClasses msg comparable
    -> State comparable
    -> comparable
    -> (i -> Maybe String)
    -> List (Comment r m i)
    -> List (Html msg)
viewCommentList config state key getName comments =
    let
        class value =
            Html.Attributes.class <| config.cssConfig.toString value

        tailEditor =
            case Dict.get key state.newCommentDrafts of
                Just draft ->
                    [ Html.map config.toMsg <| newCommentEditor config key draft ]

                Nothing ->
                    [ Html.map config.toMsg <| newCommentEditor config key defaultCommentDraft ]
    in
    (List.concat <|
        List.indexedMap
            (\index value ->
                if value.metadata.isDeleted then
                    [ text "" ]

                else
                    [ viewComment config state key index getName value
                    , div [ class config.cssConfig.spacer ] []
                    ]
            )
            comments
    )
        ++ tailEditor


{-| -}
viewComment :
    Config cssClasses msg comparable
    -> State comparable
    -> comparable
    -> Int
    -> (i -> Maybe String)
    -> Comment r m i
    -> Html msg
viewComment config state key commentIndex getName comment =
    let
        class value =
            Html.Attributes.class <| config.cssConfig.toString value

        selector =
            Existing key commentIndex

        {- if a draft exists then we want to edit that instead -}
        ( editing, draftComment ) =
            Dict.get key state.editCommentDrafts
                |> Maybe.andThen (Dict.get commentIndex)
                |> (\m ->
                        case m of
                            Just draft ->
                                ( True, draft )

                            Nothing ->
                                -- we should never acutally use the defaultCommentDraft
                                ( False, defaultCommentDraft )
                   )

        username =
            Maybe.withDefault "Anonymous" (getName comment.metadata.createdBy)
    in
    if editing then
        Html.map config.toMsg <| updateCommentEditor config key commentIndex draftComment

    else
        -- TODO figure out this weirdness where we need to comment out the mapping
        Html.map config.toMsg
            (form [ class config.cssConfig.editor ]
                [ div [ class config.cssConfig.header ]
                    -- the top toolbar element
                    [ span []
                        [ a []
                            [ text username ]
                        , text " commented "
                        , text <| Iso8601.fromTime comment.metadata.createdTimestamp
                        ]
                    , span []
                        [ button
                            [ Html.Events.custom
                                "click"
                                (Json.Decode.succeed <|
                                    { message = Internal <| CreateCommentDraft selector
                                    , stopPropagation = True
                                    , preventDefault = True
                                    }
                                )
                            , class config.cssConfig.toolbarButton
                            ]
                            [ Octicons.pencil { octiconsDefaultOptions | width = 14 } ]
                        , button
                            [ Html.Events.custom
                                "click"
                                (Json.Decode.succeed <|
                                    { message = DeleteComment key commentIndex
                                    , stopPropagation = True
                                    , preventDefault = True
                                    }
                                )
                            , class config.cssConfig.toolbarButton
                            ]
                            [ Octicons.trashcan { octiconsDefaultOptions | width = 12 } ]
                        ]
                    ]
                , Markdown.toHtmlWith
                    { githubFlavored =
                        Just { tables = False, breaks = False }
                    , defaultHighlighting = Just "elm"
                    , sanitize = True
                    , smartypants = False
                    }
                    [ class config.cssConfig.markdownBody ]
                    comment.markdown
                ]
            )



{- Why do we chose to use indirection in NewComment? Because
   we want to avoid registering a new onclick event event every
   time the contents of the comment are updated.
-}


newCommentEditor : Config cssClasses msg key -> key -> CommentDraft -> Html (Msg key)
newCommentEditor config key =
    let
        selector : CommentDraftSelector key
        selector =
            New key
    in
    commentEditor
        config
        ( ( translate config.translations "comment-editor-discard-draft", Json.Decode.succeed <| Internal <| DeleteCommentDraft selector )
        , ( translate config.translations "comment-editor-comment", Json.Decode.succeed <| NewComment key )
        )
        selector


updateCommentEditor : Config cssClasses msg key -> key -> Int -> CommentDraft -> Html (Msg key)
updateCommentEditor config key commentIndex =
    let
        selector : CommentDraftSelector key
        selector =
            Existing key commentIndex
    in
    commentEditor
        config
        ( ( translate config.translations "comment-editor-cancel", Json.Decode.succeed <| Internal <| DeleteCommentDraft selector )
        , ( translate config.translations "comment-editor-update-comment", Json.Decode.succeed <| UpdateComment key commentIndex )
        )
        selector


viewToolbar : Config cssClasses m k -> Bool -> Html msg
viewToolbar config showMarkdownPreview =
    let
        class value =
            Html.Attributes.class <| config.cssConfig.toString value
    in
    -- hide the toolbar when previewing
    div [] <|
        if showMarkdownPreview then
            []

        else
            [ span
                [ class config.cssConfig.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedSE, True ) ]
                , Html.Attributes.attribute "aria-label" <| translate config.translations "comment-editor-tooltip-text-size"
                ]
                [ Octicons.textSize { octiconsDefaultOptions | width = 18 } ]
            , span
                [ class config.cssConfig.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedSE, True ) ]
                , Html.Attributes.attribute "aria-label" <| translate config.translations "comment-editor-tooltip-bold-text"
                ]
                [ Octicons.bold { octiconsDefaultOptions | width = 10 } ]
            , span
                [ class config.cssConfig.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" <| translate config.translations "comment-editor-tooltip-italic-text"
                ]
                [ Octicons.italic { octiconsDefaultOptions | width = 6 } ]
            , span
                [ class config.cssConfig.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" <| translate config.translations "comment-editor-tooltip-code"
                ]
                [ Octicons.code { octiconsDefaultOptions | width = 14 } ]
            , span
                [ class config.cssConfig.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" <| translate config.translations "comment-editor-tooltip-quote"
                ]
                [ Octicons.quote { octiconsDefaultOptions | width = 14 } ]
            , span
                [ class config.cssConfig.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" <| translate config.translations "comment-editor-tooltip-bulleted-list"
                ]
                [ Octicons.listUnordered { octiconsDefaultOptions | width = 12 } ]
            , span
                [ class config.cssConfig.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" <| translate config.translations "comment-editor-tooltip-numbered-list"
                ]
                [ Octicons.listOrdered { octiconsDefaultOptions | width = 12 } ]
            , span
                [ class config.cssConfig.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" <| translate config.translations "comment-editor-tooltip-task-list"
                ]
                [ Octicons.tasklist { octiconsDefaultOptions | width = 16 } ]
            ]


type alias CommentActions key =
    ( ( String, Json.Decode.Decoder (Msg key) ), ( String, Json.Decode.Decoder (Msg key) ) )


{-| The state should produce a piece of valid HTML that will work without
javascript, so that when the server side rendered initial page state is sent
to the user, they can still interact with it. The state is parameterized by
the list of form action buttons, so that the state component can be reused
for create and update. Actual button elements are used so we can use a server
side fallback when the client does not run JS. TODO: fill in formaction on
the buttons and write server code to accomplish this.
-}
commentEditor : Config cssClasses msg key -> CommentActions key -> CommentDraftSelector key -> CommentDraft -> Html (Msg key)
commentEditor config ( ( leftText, leftDecoder ), ( rightText, rightDecoder ) ) selector draft =
    let
        toString =
            config.cssConfig.toString

        class value =
            Html.Attributes.class <| config.cssConfig.toString value

        editorBody : Html (Msg key)
        editorBody =
            if draft.showMarkdownPreview then
                Markdown.toHtmlWith
                    { githubFlavored =
                        Just { tables = False, breaks = False }
                    , defaultHighlighting = Just "elm"
                    , sanitize = True
                    , smartypants = False
                    }
                    [ class config.cssConfig.markdownBody ]
                    draft.markdown

            else
                div [ class config.cssConfig.body ]
                    [ textarea
                        [ tabindex 1
                        , placeholder <| translate config.translations "comment-editor-placeholder"
                        , class config.cssConfig.textInput
                        , onInput (Internal << UpdateCommentDraft selector)
                        , onFocus (Internal <| CreateCommentDraft selector)
                        , Html.Attributes.value draft.markdown
                        ]
                        []
                    ]
    in
    form [ class config.cssConfig.editor ]
        [ div [ class config.cssConfig.header ]
            -- the top toolbar element
            [ viewToolbar config draft.showMarkdownPreview
            , nav [ class config.cssConfig.horizontalTabNav ]
                [ button
                    [ Html.Attributes.classList
                        [ ( toString config.cssConfig.horizontalTabSelected, not draft.showMarkdownPreview )
                        , ( toString config.cssConfig.horizontalTab, True )
                        ]
                    , Html.Attributes.attribute "role" "tab"
                    , Html.Events.custom
                        "click"
                        (Json.Decode.succeed <|
                            { message = Internal <| PreviewCommentDraft selector False
                            , stopPropagation = True
                            , preventDefault = True
                            }
                        )
                    ]
                    [ text <| translate config.translations "comment-editor-tab-write" ]
                , button
                    [ Html.Attributes.classList
                        [ ( toString config.cssConfig.horizontalTabSelected, draft.showMarkdownPreview )
                        , ( toString config.cssConfig.horizontalTab, True )
                        ]
                    , Html.Attributes.attribute "role" "tab"
                    , Html.Events.custom
                        "click"
                        (Json.Decode.succeed <|
                            { message = Internal <| PreviewCommentDraft selector True
                            , stopPropagation = True
                            , preventDefault = True
                            }
                        )
                    ]
                    [ text <| translate config.translations "comment-editor-tab-preview" ]
                ]
            ]
        , editorBody
        , div [ class config.cssConfig.footer ] <|
            [ span []
                [ Octicons.markdown Octicons.defaultOptions
                , text <| translate config.translations "comment-editor-markdown-supported"
                ]
            , span []
                [ button
                    [ tabindex 3
                    , class config.cssConfig.button
                    , Html.Events.custom
                        "click"
                        (Json.Decode.andThen
                            (\m ->
                                Json.Decode.succeed
                                    { message = m
                                    , stopPropagation = True
                                    , preventDefault = True
                                    }
                            )
                            leftDecoder
                        )
                    ]
                    [ text leftText ]
                , button
                    [ tabindex 2
                    , class config.cssConfig.button
                    , Html.Events.custom
                        "click"
                        (Json.Decode.andThen
                            (\m ->
                                Json.Decode.succeed
                                    { message = m
                                    , stopPropagation = True
                                    , preventDefault = True
                                    }
                            )
                            rightDecoder
                        )
                    ]
                    [ text rightText ]
                ]
            ]
        ]
