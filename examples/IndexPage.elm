module IndexPage exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)

-- ours

viewHead : List (Html msg) -> Html msg
viewHead resources =
    node "head" [ lang "en" ]
        <|
        [ node "meta" [ charset "utf-8" ] []
        , node "meta" [ name "viewport", content "width=device-width" ] []
        , node "title" [] [ text "Elm Comments Example" ]
        , node "meta"
            [ name "description"
            , content "Demo of elm markdown comments widget"
            ]
            []
        ]
        ++ resources


linkStylesheet : String -> Html msg
linkStylesheet ref =
    node "link"
       [ rel "stylesheet"
       , href ref
       , media "all"
       ]
       []

view : Html msg
view =
    node "html" []
        [ viewHead
            [ node "script"
                [ type_ "text/javascript"
                , src "example.js"
                ] []
            , linkStylesheet "example.css"
            , linkStylesheet "highlight.min.css"
            
            ]
        , viewBody
        ]


viewBody : Html msg
viewBody =
    node "body" []
        [ node "script" []
            [ text """
Elm.Example.fullscreen();
"""
            ]
        ]

