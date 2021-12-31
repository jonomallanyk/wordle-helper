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
      , selectedLetterPosition =
            Nothing
      , newChar =
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
    , selectedLetterPosition : Maybe Position
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
    | SubmittedWord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectedLetter letter ->
            ( case letter.position of
                First ->
                    { model | selectedLetterPosition = Just First }

                Second ->
                    { model | selectedLetterPosition = Just Second }

                Third ->
                    { model | selectedLetterPosition = Just Third }

                Fourth ->
                    { model | selectedLetterPosition = Just Fourth }

                Fifth ->
                    { model | selectedLetterPosition = Just Fifth }
            , Cmd.none
            )

        UpdatedChar char ->
            case model.selectedLetterPosition of
                Nothing ->
                    ( model, Cmd.none )

                Just selectedPosition ->
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
                            case selectedPosition of
                                First ->
                                    { model
                                        | firstLetter =
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
                                    }

                                Third ->
                                    { model
                                        | thirdLetter =
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
                                    }

                                Fifth ->
                                    { model
                                        | fifthLetter =
                                            { fifthLetter
                                                | char = Just char
                                            }
                                    }
                    in
                    ( { updatedModel
                        | selectedLetterPosition =
                            findNextUnlockedPosition
                                [ firstLetter
                                , secondLetter
                                , thirdLetter
                                , fourthLetter
                                , fifthLetter
                                ]
                                model.selectedLetterPosition
                      }
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

        SubmittedWord ->
            ( model, Cmd.none )


lettersToWord : Letter -> Letter -> Letter -> Letter -> Letter -> Maybe String
lettersToWord first second third fourth fifth =
    let
        submittedString =
            letterCharToString first ++ letterCharToString second ++ letterCharToString third ++ letterCharToString fourth ++ letterCharToString fifth
    in
    if String.length submittedString == 5 then
        Just submittedString

    else
        Nothing


letterCharToString : Letter -> String
letterCharToString letter =
    case letter.char of
        Nothing ->
            ""

        Just char ->
            String.fromChar char


{-| Move the cursor to the next open letter position
-}
findNextUnlockedPosition : List Letter -> Maybe Position -> Maybe Position
findNextUnlockedPosition letters activePosition =
    let
        filteredList =
            case activePosition of
                Nothing ->
                    List.filter (\l -> l.status /= Locked) letters

                Just First ->
                    List.filter
                        (\l -> l.status /= Locked)
                        (List.drop 1 letters ++ List.take 1 letters)

                Just Second ->
                    List.filter
                        (\l -> l.status /= Locked)
                        (List.drop 2 letters ++ List.take 2 letters)

                Just Third ->
                    List.filter
                        (\l -> l.status /= Locked)
                        (List.drop 3 letters ++ List.take 3 letters)

                Just Fourth ->
                    List.filter
                        (\l -> l.status /= Locked)
                        (List.drop 4 letters ++ List.take 4 letters)

                Just Fifth ->
                    List.filter (\l -> l.status /= Locked) letters
    in
    if List.isEmpty filteredList then
        Nothing

    else
        case List.head filteredList of
            Nothing ->
                Nothing

            Just letter ->
                Just letter.position



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
            [ viewLetterBox model.selectedLetterPosition model.firstLetter
            , viewLetterBox model.selectedLetterPosition model.secondLetter
            , viewLetterBox model.selectedLetterPosition model.thirdLetter
            , viewLetterBox model.selectedLetterPosition model.fourthLetter
            , viewLetterBox model.selectedLetterPosition model.fifthLetter
            ]
        , p
            []
            [ text ("Current possible words: " ++ (List.length model.activeWords |> toString)) ]
        ]


viewLetterBox : Maybe Position -> Letter -> Html Msg
viewLetterBox maybePosition letter =
    let
        isSelected =
            case maybePosition of
                Nothing ->
                    False

                Just position ->
                    position == letter.position

        selectedStyle =
            case maybePosition of
                Nothing ->
                    ""

                Just position ->
                    if position == letter.position then
                        case letter.status of
                            Unknown ->
                                " border-8 border-slate-700"

                            Incorrect ->
                                " border-8 border-slate-700"

                            Correct ->
                                " border-8 border-green-700"

                            Locked ->
                                ""

                    else
                        ""

        statusStyle =
            case letter.status of
                Unknown ->
                    " border-8 border-slate-300"

                Incorrect ->
                    " bg-slate-500 border-slate-300 text-white"

                Correct ->
                    " bg-green-500 text-white"

                Locked ->
                    " bg-green-500 border-0 text-white"
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


{-| Capture keyboard events

Useful reference: <https://stackoverflow.com/questions/44383764/how-to-listen-for-both-keypress-and-keydown-events-in-elm>

-}
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyUp keyDecoder
