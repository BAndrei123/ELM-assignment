module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time
import Html exposing (a)


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}
sortFromString : String -> Maybe SortBy
sortFromString string =
     case string of
         "Score" -> Just Score
         "Invalid" -> Nothing
         "Title" -> Just Title
         _ -> Nothing


sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


{-| A type that describes what option changed and how
-}
type Change
    = ChangedPostsToFetch Int | PostsToShowChanged Int | ChangedSortBy SortBy | ChangedshowJobs Bool | ChangedShowTextOnly Bool




{-| Given a change and the current configuration, return a new configuration with the changes applied
-}

-- am adaugat eu asta
applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change postsConfig =
        case change of
            ChangedPostsToFetch value ->
                { postsConfig | postsToFetch = value }

            PostsToShowChanged value ->
                { postsConfig | postsToShow = value }

            ChangedSortBy value ->
                { postsConfig | sortBy = value }

            ChangedshowJobs value ->
                { postsConfig | showJobs = value }

            ChangedShowTextOnly value ->
                { postsConfig | showTextOnly = value }     
        




{-| Given the configuration and a list of posts, return the relevant subset of posts according to the configuration

Relevant local functions:

  - sortToCompareFn

Relevant library functions:

  - List.sortWith

-}
filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    let
        textPosts: Post -> Bool
        textPosts post = 
            if config.showTextOnly == False  then
                case post.url of
                    Just u -> True
                    Nothing -> False
            else 
            case post.url of
                Just u -> True
                Nothing -> True

        jobPosts: Post -> Bool
        jobPosts post =
            if config.showJobs == True then
                 post.type_ == "job"
            else
                 post.type_ /= "job"
    
        filteredPosts =
            posts |> List.filter (\post -> textPosts post) |> List.filter (\post -> jobPosts post)
        
        finalListTake =
            List.take config.postsToShow filteredPosts

        finalPosts =
            List.sortWith (sortToCompareFn config.sortBy) finalListTake
    
    in
        finalPosts