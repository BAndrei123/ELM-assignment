module View.Posts exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Model.PostsConfig exposing(..)
import Time
import Util.Time
import Html exposing (table)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config currentTime posts =
    div[][
      table [][
        thead[][
          tr[][
             th [] [ text "Score" ]
                    , th [] [ text "Title" ]
                    , th [] [ text "Link" ]
                    , th [] [ text "Type" ]
                    , th [] [ text "Posted Date" ]
                    
          ]
        ]
      , tbody [] ( Model.PostsConfig.filterPosts config posts |>List.map (postToHtml currentTime)  )
      ]
    ]

postToHtml : Time.Posix -> Post -> Html Msg
postToHtml currentTime post  =
  
    tr []
      [
        td [  class "post-score" ] [ text <| String.fromInt post.score ]
        , td [ class "post-title" ] [ text post.title ]
          , td [  class "post-url" ]
              [ case post.url of
                  Just u -> a [ href u, class "post-url" ] [ text "Link" ]
                  Nothing -> text ""
              ]
          , td [  class "post-type" ] [ text post.type_ ]
          , td [  class "post-time" ] [ text ((Util.Time.formatTime Time.utc (post.time) ++ " (" ++ (Maybe.withDefault { days = 0, hours = 0, minutes = 0, seconds = 0 } (Util.Time.durationBetween post.time currentTime) |> Util.Time.formatDuration) ++ ")"))  ]
          
      ]
   
{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
   div []
        [ div[][select [ id "select-posts-per-page" , onInput (String.toInt >> Maybe.withDefault 0 >> PostsToShowChanged >> ConfigChanged) ] -- de modificat cu message
            [ option [ value "10" ] [ text "10"  ]
            , option [ value "25" ] [ text "25" ]
            , option [ value "50" ] [ text "50" ]
            ]
        , select [ id "select-sort-by" , onInput ( sortFromString >> Maybe.withDefault None >> ChangedSortBy >> ConfigChanged) ]
            [ option [ value "score"] [ text "Score" ]
            , option [ value "title" ] [ text "Title" ]
            , option [ value "Posted"] [ text "Posted" ]
            , option [ value "None" ] [ text "None" ]
            ]]
        , div[][input [id "checkbox-show-job-posts", type_ "checkbox", onCheck (ChangedshowJobs >> ConfigChanged) ][]
        , text "Show job"
        ,input [id "checkbox-show-text-only-posts", type_ "checkbox",  onCheck (ChangedShowTextOnly >> ConfigChanged)][]
        , text "Show text"]]


