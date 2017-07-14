module Comments exposing (Msg(..), State, defaultState, update, viewComment, viewCommentList, getEditDraft, getNewDraft, removeEditDraft, removeNewDraft)

{-| Standalone github style markdown comments editor

@docs Msg, State, defaultState, update, viewComment, viewCommentList

@docs getEditDraft, getNewDraft, removeEditDraft, removeNewDraft
-}

import Html exposing (Html, div, text, h2, p, nav, button, span, form, ul, li, label, input, a, textarea)
import Html.Attributes exposing (href, type_, style, for, checked, placeholder, tabindex)
import Html.Events exposing (onInput, onClick, onFocus, onWithOptions, Options)
import Html.Keyed
import Json.Decode
import Dict exposing (Dict)


-- third party

import Markdown
import Octicons
import Css.Primer.Tooltips.Selectors as Tooltips
import Css.Primer.Tooltips.Selectors exposing (CssClasses(..))


-- ours

import Comments.Css exposing (Config)


octiconsDefaultOptions =
    Octicons.defaultOptions



-- MODEL


{-|
-}
type alias State key =
    { newCommentDrafts : Dict key CommentDraft
    , editCommentDrafts : Dict key (Dict Int CommentDraft)
    }


{-|
-}
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
            (Maybe.withDefault
                Dict.empty
                (Dict.get key state.editCommentDrafts)
            )

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


{-|
-}
getEditDraft : comparable -> Int -> State comparable -> Maybe String
getEditDraft key commentIndex state =
    Dict.get key state.editCommentDrafts
        |> Maybe.andThen (Dict.get commentIndex)
        |> Maybe.andThen (\x -> Just x.markdown)


{-|
-}
removeEditDraft : comparable -> Int -> State comparable -> State comparable
removeEditDraft key commentIndex state =
    let
        oldRow =
            (Maybe.withDefault
                Dict.empty
                (Dict.get key state.editCommentDrafts)
            )
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


{-|
-}
getNewDraft : comparable -> State comparable -> Maybe String
getNewDraft key state =
    Dict.get key state.newCommentDrafts
        |> Maybe.andThen (\x -> Just x.markdown)


{-|
-}
removeNewDraft : comparable -> State comparable -> State comparable
removeNewDraft key state =
    { state | newCommentDrafts = Dict.remove key state.newCommentDrafts }



-- UPDATE


{-|
-}
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
                    removeEditDraft key commentIndex state ! []

                New key ->
                    { state | newCommentDrafts = Dict.remove key state.newCommentDrafts }
                        ! []

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
                            state ! []

                        Nothing ->
                            case createDraftFromExisting key commentIndex of
                                Just commentToEdit ->
                                    insertEditDraft key commentIndex commentToEdit state
                                        ! []

                                Nothing ->
                                    state ! []

                New key ->
                    if Dict.member key state.newCommentDrafts then
                        state ! []
                    else
                        { state
                            | newCommentDrafts =
                                Dict.insert
                                    key
                                    defaultCommentDraft
                                    state.newCommentDrafts
                        }
                            ! []

        UpdateCommentDraft selector value ->
            case selector of
                Existing key commentIndex ->
                    { state
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
                        ! []

                New key ->
                    { state
                        | newCommentDrafts =
                            Dict.update
                                key
                                (Maybe.map <| setDraftMarkdown value)
                                state.newCommentDrafts
                    }
                        ! []

        PreviewCommentDraft selector showMarkdownPreview ->
            case selector of
                Existing key commentIndex ->
                    { state
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
                        ! []

                New key ->
                    { state
                        | newCommentDrafts =
                            Dict.update
                                key
                                (Maybe.map <| setDraftPreviewFlag showMarkdownPreview)
                                state.newCommentDrafts
                    }
                        ! []



-- VIEW


type alias Comment r m =
    { r
        | markdown : String
        , metadata :
            { m
                | createdBy : String
                , createdTimestamp : String
                , isDeleted : Bool
            }
    }


{-|
-}
viewCommentList : (Msg comparable -> msg) -> Config cssClasses -> State comparable -> comparable -> List (Comment r m) -> List (Html msg)
viewCommentList toMsg config state key comments =
    let
        tailEditor =
            case Dict.get key state.newCommentDrafts of
                Just draft ->
                    [ Html.map toMsg <| newCommentEditor config key draft ]

                Nothing ->
                    [ Html.map toMsg <| newCommentEditor config key defaultCommentDraft ]
    in
        (List.concat <|
            (List.indexedMap
                (\index value ->
                    if value.metadata.isDeleted then
                        [ text "" ]
                    else
                        [ viewComment toMsg config state key index value
                        , div [ class config.spacer ] []
                        ]
                )
                comments
            )
        )
            ++ tailEditor


{-|
-}
viewComment : (Msg comparable -> msg) -> Config cssClasses -> State comparable -> comparable -> Int -> Comment r m -> Html msg
viewComment toMsg config state key commentIndex comment =
    let
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
    in
        if editing then
            Html.map toMsg <| updateCommentEditor config key commentIndex draftComment
        else
            Html.map toMsg <|
                form [ class config.editor ]
                    [ div [ class config.header ]
                        -- the top toolbar element
                        [ span []
                            [ a []
                                [ text comment.metadata.createdBy ]
                            , text " commented "
                            , text comment.metadata.createdTimestamp
                            ]
                        , span []
                            [ button
                                [ onWithOptions
                                    "click"
                                    (Options True True)
                                    (Json.Decode.succeed <| Internal <| CreateCommentDraft selector)
                                , class config.toolbarButton
                                ]
                                [ Octicons.pencil { octiconsDefaultOptions | width = 14 } ]
                            , button
                                [ onWithOptions
                                    "click"
                                    (Options True True)
                                    (Json.Decode.succeed <| DeleteComment key commentIndex)
                                , class config.toolbarButton
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
                        [ class config.markdownBody ]
                        comment.markdown
                    ]



{- Why do we chose to use indirection in NewComment? Because
   we want to avoid registering a new onclick event event every
   time the contents of the comment are updated.
-}


newCommentEditor : Config cssClasses -> key -> CommentDraft -> Html (Msg key)
newCommentEditor config key =
    let
        selector : CommentDraftSelector key
        selector =
            New key
    in
        commentEditor
            config
            ( ( "Discard Draft", Json.Decode.succeed <| Internal <| DeleteCommentDraft selector )
            , ( "Comment", Json.Decode.succeed <| NewComment key )
            )
            selector


updateCommentEditor : Config cssClasses -> key -> Int -> CommentDraft -> Html (Msg key)
updateCommentEditor config key commentIndex =
    let
        selector : CommentDraftSelector key
        selector =
            Existing key commentIndex
    in
        commentEditor
            config
            ( ( "Cancel", Json.Decode.succeed <| Internal <| DeleteCommentDraft selector )
            , ( "Update Comment", Json.Decode.succeed <| UpdateComment key commentIndex )
            )
            selector


viewToolbar : Config cssClasses -> Bool -> Html msg
viewToolbar config showMarkdownPreview =
    -- hide the toolbar when previewing
    div [] <|
        if showMarkdownPreview then
            []
        else
            [ span
                [ class config.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedSE, True ) ]
                , Html.Attributes.attribute "aria-label" "text size"
                ]
                [ Octicons.textSize { octiconsDefaultOptions | width = 18 } ]
            , span
                [ class config.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedSE, True ) ]
                , Html.Attributes.attribute "aria-label" "bold text"
                ]
                [ Octicons.bold { octiconsDefaultOptions | width = 10 } ]
            , span
                [ class config.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" "italic text"
                ]
                [ Octicons.italic { octiconsDefaultOptions | width = 6 } ]
            , span
                [ class config.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" "code"
                ]
                [ Octicons.code { octiconsDefaultOptions | width = 14 } ]
            , span
                [ class config.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" "quote"
                ]
                [ Octicons.quote { octiconsDefaultOptions | width = 14 } ]
            , span
                [ class config.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" "bulleted list"
                ]
                [ Octicons.listUnordered { octiconsDefaultOptions | width = 12 } ]
            , span
                [ class config.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" "numbered list"
                ]
                [ Octicons.listOrdered { octiconsDefaultOptions | width = 12 } ]
            , span
                [ class config.toolbarButton
                , Tooltips.classList [ ( ToolTipped, True ), ( ToolTippedS, True ) ]
                , Html.Attributes.attribute "aria-label" "task list"
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
commentEditor : Config cssClasses -> CommentActions key -> CommentDraftSelector key -> CommentDraft -> Html (Msg key)
commentEditor config ( ( leftText, leftDecoder ), ( rightText, rightDecoder ) ) selector draft =
    let
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
                    [ class config.markdownBody ]
                    draft.markdown
            else
                div [ class config.body ]
                    [ textarea
                        [ tabindex 1
                        , placeholder "Leave a comment"
                        , class config.textInput
                        , onInput (Internal << UpdateCommentDraft selector)
                        , onFocus (Internal <| CreateCommentDraft selector)
                        , Html.Attributes.value draft.markdown
                        ]
                        []
                    ]
    in
        form [ class config.editor ]
            [ div [ class config.header ]
                -- the top toolbar element
                [ viewToolbar config draft.showMarkdownPreview
                , nav [ class config.horizontalTabNav ]
                    [ button
                        [ Html.Attributes.classList
                            [ ( toString config.horizontalTabSelected, not draft.showMarkdownPreview )
                            , ( toString config.horizontalTab, True )
                            ]
                        , Html.Attributes.attribute "role" "tab"
                        , onWithOptions
                            "click"
                            (Options True True)
                            (Json.Decode.succeed <| Internal <| PreviewCommentDraft selector False)
                        ]
                        [ text "Write" ]
                    , button
                        [ Html.Attributes.classList
                            [ ( toString config.horizontalTabSelected, draft.showMarkdownPreview )
                            , ( toString config.horizontalTab, True )
                            ]
                        , Html.Attributes.attribute "role" "tab"
                        , onWithOptions
                            "click"
                            (Options True True)
                            (Json.Decode.succeed <| Internal <| PreviewCommentDraft selector True)
                        ]
                        [ text "Preview" ]
                    ]
                ]
            , editorBody
            , div [ class config.footer ] <|
                [ span []
                    [ Octicons.markdown Octicons.defaultOptions
                    , text " Markdown supported"
                    ]
                , span []
                    [ button
                        [ tabindex 3
                        , class config.button
                        , onWithOptions
                            "click"
                            (Options True True)
                            leftDecoder
                        ]
                        [ text leftText ]
                    , button
                        [ tabindex 2
                        , class config.button
                        , onWithOptions
                            "click"
                            (Options True True)
                            rightDecoder
                        ]
                        [ text rightText ]
                    ]
                ]
            ]


class : cssClasses -> Html.Attribute msg
class selector =
    Html.Attributes.class <| toString selector


onClickPreventDefault : Msg -> Html.Attribute Msg
onClickPreventDefault msg =
    onWithOptions
        "click"
        (Html.Events.Options True True)
        (Json.Decode.succeed msg)
