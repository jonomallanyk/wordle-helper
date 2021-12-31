module Main exposing (main)

import Browser
import Browser.Events
import Debug exposing (toString)
import Html exposing (Html, button, div, h1, h2, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Words exposing (wordList)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
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
      , newChar =
            Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyUp keyDecoder



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
    , newChar : Maybe Char
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
    | UpdatedChar Char
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

        UpdatedChar char ->
            case model.selectedLetter of
                Nothing ->
                    ( model, Cmd.none )

                Just selected ->
                    let
                        firstLetter =
                            model.firstLetter

                        secondLetter =
                            model.secondLetter

                        thirdLetter =
                            model.thirdLetter

                        fourthLetter =
                            model.fourthLetter

                        fifthLetter =
                            model.fifthLetter

                        updatedModel =
                            case selected.position of
                                First ->
                                    { model
                                        | firstLetter =
                                            { firstLetter
                                                | char = Just char
                                            }
                                        , selectedLetter =
                                            Just
                                                { firstLetter
                                                    | char = Just char
                                                }
                                    }

                                Second ->
                                    { model
                                        | secondLetter =
                                            { secondLetter
                                                | char = Just char
                                            }
                                        , selectedLetter =
                                            Just
                                                { secondLetter
                                                    | char = Just char
                                                }
                                    }

                                Third ->
                                    { model
                                        | thirdLetter =
                                            { thirdLetter
                                                | char = Just char
                                            }
                                        , selectedLetter =
                                            Just
                                                { thirdLetter
                                                    | char = Just char
                                                }
                                    }

                                Fourth ->
                                    { model
                                        | fourthLetter =
                                            { fourthLetter
                                                | char = Just char
                                            }
                                        , selectedLetter =
                                            Just
                                                { fourthLetter
                                                    | char = Just char
                                                }
                                    }

                                Fifth ->
                                    { model
                                        | fifthLetter =
                                            { fifthLetter
                                                | char = Just char
                                            }
                                        , selectedLetter =
                                            Just
                                                { fifthLetter
                                                    | char = Just char
                                                }
                                    }
                    in
                    ( updatedModel
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
        isSelected =
            case maybeSelected of
                Nothing ->
                    False

                Just l ->
                    l == letter

        selectedStyle =
            case maybeSelected of
                Nothing ->
                    " border-4 border-slate-300"

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
        , if isSelected then
            onClick <|
                case letter.char of
                    Nothing ->
                        NoOp

                    Just _ ->
                        ChangedStatus letter

          else
            onClick <| SelectedLetter letter
        ]
        [ p
            [ class "mt-0 pt-0 text-7xl leading-none" ]
            [ text <|
                case letter.char of
                    Nothing ->
                        ""

                    Just c ->
                        String.fromChar c
                            |> String.toUpper
            ]
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey str =
    case String.uncons str of
        Just ( char, "" ) ->
            if Char.isLower char then
                UpdatedChar char

            else
                NoOp

        _ ->
            NoOp
