module Example exposing (main)

import Html exposing (Html, div, text, ul, li, button, h3, node, p, hr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array exposing (Array)


-- third party

import Css


-- ours

import Comments exposing (Msg(..))
import Comments.Css


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    { feed = Array.fromList [ examplePost ]
    , commentState = Comments.defaultState
    }
        ! []



-- MODEL


type alias Model =
    { feed : Array Post
    , commentState : Comments.State Int
    }


type alias Post =
    { title : String
    , text : String
    , comments : Array Comment
    }


examplePost : Post
examplePost =
    { title = "Example Post"
    , text = "You can edit multiple comments at once. Try it!"
    , comments =
        Array.fromList
            [ { markdown = "# This is an example comment"
              , metadata =
                    { createdBy = "Steve"
                    , createdTimestamp = "Yesterday"
                    , isDeleted = False
                    }
              }
            ]
    }


type alias Comment =
    { markdown : String
    , metadata :
        { createdBy : String
        , createdTimestamp : String
        , isDeleted : Bool
        }
    }



-- UPDATE


type Msg
    = NoOp
    | CommentsMsg (Comments.Msg Int)
    | NewPost


draftFromExistingComment : Array Post -> Int -> Int -> String
draftFromExistingComment feed postIndex commentIndex =
    let
        comment : Maybe String
        comment =
            Array.get postIndex feed
                |> Maybe.andThen (\x -> Array.get commentIndex x.comments)
                |> Maybe.andThen (\y -> Just y.markdown)
    in
        Maybe.withDefault "" comment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CommentsMsg commentsMsg ->
            case commentsMsg of
                Internal internalMsg ->
                    let
                        ( newCommentState, commentCmd ) =
                            Comments.update
                                (draftFromExistingComment model.feed)
                                internalMsg
                                model.commentState
                    in
                        { model | commentState = newCommentState }
                            ! [ Cmd.map CommentsMsg commentCmd ]

                NewComment postIndex ->
                    case Array.get postIndex model.feed of
                        Just post ->
                            let
                                newComment =
                                    Comments.getNewDraft postIndex model.commentState

                                newPost =
                                    { post
                                        | comments =
                                            Array.push
                                                { markdown = Maybe.withDefault "" newComment
                                                , metadata =
                                                    { createdBy = "Steve"
                                                    , createdTimestamp = "Now"
                                                    , isDeleted = False
                                                    }
                                                }
                                                post.comments
                                    }
                            in
                                { model
                                    | feed = Array.set postIndex newPost model.feed
                                    , commentState =
                                        Comments.removeNewDraft
                                            postIndex
                                            model.commentState
                                }
                                    ! []

                        Nothing ->
                            model ! []

                UpdateComment postIndex commentIndex ->
                    case Array.get postIndex model.feed of
                        Just post ->
                            case Array.get commentIndex post.comments of
                                Just comment ->
                                    let
                                        updateComment =
                                            Comments.getEditDraft
                                                postIndex
                                                commentIndex
                                                model.commentState

                                        updatedPost =
                                            { post
                                                | comments =
                                                    Array.set
                                                        commentIndex
                                                        { comment | markdown = Maybe.withDefault "" updateComment }
                                                        post.comments
                                            }
                                    in
                                        { model
                                            | feed = Array.set postIndex updatedPost model.feed
                                            , commentState = Comments.removeEditDraft postIndex commentIndex model.commentState
                                        }
                                            ! []

                                Nothing ->
                                    model ! []

                        Nothing ->
                            model ! []

                DeleteComment postIndex commentIndex ->
                    case Array.get postIndex model.feed of
                        Just post ->
                            case Array.get commentIndex post.comments of
                                Just comment ->
                                    let
                                        metadata =
                                            comment.metadata

                                        updatedPost =
                                            { post
                                                | comments =
                                                    Array.set
                                                        commentIndex
                                                        { comment | metadata = { metadata | isDeleted = True } }
                                                        post.comments
                                            }
                                    in
                                        { model
                                            | feed = Array.set postIndex updatedPost model.feed
                                            , commentState = Comments.removeEditDraft postIndex commentIndex model.commentState
                                        }
                                            ! []

                                Nothing ->
                                    model ! []

                        Nothing ->
                            model ! []

        NoOp ->
            model ! []

        NewPost ->
            { model | feed = Array.push examplePost model.feed } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "20px auto" )
            , ( "padding", "20px" )
            , ( "max-width", "1080px" )
            ]
        ]
        [ node "style" [] [ text <| .css <| Css.compile [ Comments.Css.exampleStyleSheet ] ]
        , h3 [] [ text "viewCommentList Demo" ]
        , div []
            [ p []
                [ text "You can edit comments on multiple posts simultaneously" ]
            , button [ onClick NewPost ] [ text "New Post" ]
            ]
        , hr [] []
        , ul [] <| Array.toList <| Array.indexedMap (viewPost model.commentState) model.feed
        ]


viewPost : Comments.State Int -> Int -> Post -> Html Msg
viewPost commentState postIndex post =
    li
        [ style
            [ ( "max-width", "700px" )
            , ( "background-color", "#eee" )
            , ( "padding", "10px" )
            , ( "margin", "10px 0px" )
            , ( "border", "1px solid #ddd" )
            ]
        ]
    <|
        Comments.viewCommentList
            CommentsMsg
            Comments.Css.defaultConfig
            commentState
            postIndex
            (Array.toList post.comments)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
