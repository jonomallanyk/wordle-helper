module Main exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (Html, button, div, h1, h2, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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
      , firstLetter =
            { char = Nothing
            , status = Unknown
            , position = First
            }
      , secondLetter =
            { char = Nothing
            , status = Unknown
            , position = Second
            }
      , thirdLetter =
            { char = Nothing
            , status = Unknown
            , position = Third
            }
      , fourthLetter =
            { char = Nothing
            , status = Unknown
            , position = Fourth
            }
      , fifthLetter =
            { char = Nothing
            , status = Unknown
            , position = Fifth
            }
      , selectedLetter =
            Nothing
      }
    , Cmd.none
    )



-- Model


type alias Model =
    { activeWords : List String
    , activeLetters : List Char
    , eliminatedLetters : List Char
    , firstLetter : Letter
    , secondLetter : Letter
    , thirdLetter : Letter
    , fourthLetter : Letter
    , fifthLetter : Letter
    , selectedLetter : Maybe Letter
    }


type alias Letter =
    { char : Maybe Char
    , status : Status
    , position : Position
    }


type Status
    = Unknown
    | Incorrect
    | Correct
    | Locked


type Position
    = First
    | Second
    | Third
    | Fourth
    | Fifth



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
    | SelectedLetter Letter
    | ChangedStatus Letter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectedLetter letter ->
            ( case letter.position of
                First ->
                    { model | selectedLetter = Just model.firstLetter }

                Second ->
                    { model | selectedLetter = Just model.secondLetter }

                Third ->
                    { model | selectedLetter = Just model.thirdLetter }

                Fourth ->
                    { model | selectedLetter = Just model.fourthLetter }

                Fifth ->
                    { model | selectedLetter = Just model.fifthLetter }
            , Cmd.none
            )

        ChangedStatus letter ->
            let
                newLetter =
                    { letter
                        | status =
                            case letter.status of
                                Unknown ->
                                    Incorrect

                                Incorrect ->
                                    Correct

                                Correct ->
                                    Unknown

                                Locked ->
                                    Locked
                    }
            in
            ( case letter.position of
                First ->
                    { model | firstLetter = newLetter }

                Second ->
                    { model | secondLetter = newLetter }

                Third ->
                    { model | thirdLetter = newLetter }

                Fourth ->
                    { model | fourthLetter = newLetter }

                Fifth ->
                    { model | fifthLetter = newLetter }
            , Cmd.none
            )



-- fetchPosts : Cmd Msg
-- fetchPosts =
--     Http.get
--         { url = "https://jsonplaceholder.typicode.com/posts"
--         , expect = Http.expectJson (RemoteData.fromResult >> GotPostsResponse) postsDecoder
--         }
-- View


view : Model -> Html Msg
view model =
    div [ class "p-6" ]
        [ h1 [ class "mb-5" ] [ text "Wordle Helper" ]
        , div
            [ class "flex mb-6 space-x-2" ]
            [ viewLetterBox model.selectedLetter model.firstLetter
            , viewLetterBox model.selectedLetter model.secondLetter
            , viewLetterBox model.selectedLetter model.thirdLetter
            , viewLetterBox model.selectedLetter model.fourthLetter
            , viewLetterBox model.selectedLetter model.fifthLetter
            ]
        , p
            []
            [ text ("Current possible words: " ++ (List.length model.activeWords |> toString)) ]
        ]


viewLetterBox : Maybe Letter -> Letter -> Html Msg
viewLetterBox maybeSelected letter =
    let
        selectedStyle =
            case maybeSelected of
                Nothing ->
                    ""

                Just selected ->
                    if selected == letter then
                        " border-4 border-slate-700"

                    else
                        " border-4 border-slate-300"

        statusStyle =
            case letter.status of
                Unknown ->
                    ""

                Incorrect ->
                    " bg-slate-500 border-4 border-slate-700 text-white"

                Correct ->
                    " bg-green-500 border-4 border-green-700 text-white"

                Locked ->
                    " bg-green-500 border-4 border-green-700 text-white"
    in
    div
        [ class ("p-4 text-center w-24 h-24 flex items-center justify-center" ++ statusStyle ++ selectedStyle)
        , onClick <|
            case letter.char of
                Nothing ->
                    NoOp

                Just _ ->
                    ChangedStatus letter
        ]
        [ p
            [ class "mt-0 pt-0 text-7xl leading-none" ]
            [ text <|
                case letter.char of
                    Nothing ->
                        ""

                    Just c ->
                        String.fromChar c
            ]
        ]
