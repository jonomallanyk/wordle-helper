module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, p, text)
import Words exposing (wordList)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { activeWords = wordList
      , activeLetters = []
      , eliminatedLetters = []
      }
    , Cmd.none
    )



-- Model


type alias Model =
    { activeWords : List String

    -- , currentLetters : List Char
    , activeLetters : List Char
    , eliminatedLetters : List Char
    }



-- postsDecoder : Json.Decode.Decoder (List Post)
-- postsDecoder =
--     Json.Decode.list
--         (Json.Decode.map3
--             Post
--             (Json.Decode.field "id" Json.Decode.int)
--             (Json.Decode.field "title" Json.Decode.string)
--             (Json.Decode.field "body" Json.Decode.string)
--         )
-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- fetchPosts : Cmd Msg
-- fetchPosts =
--     Http.get
--         { url = "https://jsonplaceholder.typicode.com/posts"
--         , expect = Http.expectJson (RemoteData.fromResult >> GotPostsResponse) postsDecoder
--         }
-- View


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Wordle Helper" ]
        ]
