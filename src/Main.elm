module Main exposing (main)

import Browser
import Browser.Events
import Debug exposing (toString)
import Html exposing (Html, button, div, h1, h2, li, ol, p, span, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Letter exposing (Letter, Status(..), initLetters, updateLetters)
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
      , letters = initLetters
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
      , guessedWord =
            [ '_'
            , '_'
            , '_'
            , '_'
            , '_'
            ]
      , showWordList = False
      }
    , Cmd.none
    )



-- Model


type alias Model =
    { activeWords : List String
    , letters : List Letter
    , firstLetter : Guess
    , secondLetter : Guess
    , thirdLetter : Guess
    , fourthLetter : Guess
    , fifthLetter : Guess
    , selectedLetterPosition : Maybe Position
    , guessedWord : List Char
    , showWordList : Bool
    }


type alias Guess =
    { char : Maybe Char
    , status : Status
    , position : Position
    }


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
    | SelectedLetter Guess
    | UpdatedChar Char
    | ChangedStatus Guess
    | SubmittedWord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
    in
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
                        updatedModel =
                            case selectedPosition of
                                First ->
                                    { model
                                        | firstLetter =
                                            { firstLetter
                                                | char = Just char
                                                , status = Incorrect
                                            }
                                    }

                                Second ->
                                    { model
                                        | secondLetter =
                                            { secondLetter
                                                | char = Just char
                                                , status = Incorrect
                                            }
                                    }

                                Third ->
                                    { model
                                        | thirdLetter =
                                            { thirdLetter
                                                | char = Just char
                                                , status = Incorrect
                                            }
                                    }

                                Fourth ->
                                    { model
                                        | fourthLetter =
                                            { fourthLetter
                                                | char = Just char
                                                , status = Incorrect
                                            }
                                    }

                                Fifth ->
                                    { model
                                        | fifthLetter =
                                            { fifthLetter
                                                | char = Just char
                                                , status = Incorrect
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
                nextStatus =
                    { letter
                        | status =
                            case letter.status of
                                Unknown ->
                                    Incorrect

                                Incorrect ->
                                    Correct

                                Correct ->
                                    Incorrect

                                Locked ->
                                    Locked
                    }
            in
            ( case letter.position of
                First ->
                    { model | firstLetter = nextStatus }

                Second ->
                    { model | secondLetter = nextStatus }

                Third ->
                    { model | thirdLetter = nextStatus }

                Fourth ->
                    { model | fourthLetter = nextStatus }

                Fifth ->
                    { model | fifthLetter = nextStatus }
            , Cmd.none
            )

        SubmittedWord ->
            let
                lockOrResetGuess : Guess -> Guess
                lockOrResetGuess guess =
                    if guess.status == Correct then
                        { guess
                            | status = Locked
                        }

                    else if guess.status == Locked then
                        guess

                    else
                        { guess
                            | char = Nothing
                            , status = Unknown
                        }

                extractChar guess =
                    case guess.char of
                        Nothing ->
                            '_'

                        Just c ->
                            case guess.status of
                                Unknown ->
                                    '_'

                                Incorrect ->
                                    c

                                _ ->
                                    c

                newLetters =
                    model.letters
                        |> Letter.updateLetters (extractChar model.firstLetter) model.firstLetter.status
                        |> Letter.updateLetters (extractChar secondLetter) secondLetter.status
                        |> Letter.updateLetters (extractChar thirdLetter) thirdLetter.status
                        |> Letter.updateLetters (extractChar fourthLetter) fourthLetter.status
                        |> Letter.updateLetters (extractChar fifthLetter) fifthLetter.status
            in
            if allLettersSet firstLetter secondLetter thirdLetter fourthLetter fifthLetter then
                case lettersToWord firstLetter secondLetter thirdLetter fourthLetter fifthLetter of
                    Just word ->
                        ( { model
                            | guessedWord =
                                updateGuessedWord firstLetter secondLetter thirdLetter fourthLetter fifthLetter model.guessedWord
                            , firstLetter = lockOrResetGuess model.firstLetter
                            , secondLetter = lockOrResetGuess model.secondLetter
                            , thirdLetter = lockOrResetGuess model.thirdLetter
                            , fourthLetter = lockOrResetGuess model.fourthLetter
                            , fifthLetter = lockOrResetGuess model.fifthLetter
                            , selectedLetterPosition = Nothing
                            , letters = newLetters
                            , activeWords =
                                model.activeWords
                                    |> Words.removeByLetterInAnyPosition (extractChar (lockOrResetGuess model.firstLetter))
                                    |> Words.removeByLetterInAnyPosition (extractChar (lockOrResetGuess model.secondLetter))
                                    |> Words.removeByLetterInAnyPosition (extractChar (lockOrResetGuess model.thirdLetter))
                                    |> Words.removeByLetterInAnyPosition (extractChar (lockOrResetGuess model.fourthLetter))
                                    |> Words.removeByLetterInAnyPosition (extractChar (lockOrResetGuess model.fifthLetter))
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )


allLettersSet : Guess -> Guess -> Guess -> Guess -> Guess -> Bool
allLettersSet first second third fourth fifth =
    List.any
        (\i -> i.status == Unknown)
        [ first
        , second
        , third
        , fourth
        , fifth
        ]
        |> not


lettersToWord : Guess -> Guess -> Guess -> Guess -> Guess -> Maybe String
lettersToWord first second third fourth fifth =
    let
        submittedString =
            letterCharToString first ++ letterCharToString second ++ letterCharToString third ++ letterCharToString fourth ++ letterCharToString fifth
    in
    if String.length submittedString == 5 then
        Just submittedString

    else
        Nothing



-- eliminateLetters : String -> List Char
-- elinimateLetters str =


letterCharToString : Guess -> String
letterCharToString letter =
    case letter.char of
        Nothing ->
            ""

        Just char ->
            String.fromChar char


updateGuessedWord : Guess -> Guess -> Guess -> Guess -> Guess -> List Char -> List Char
updateGuessedWord first second third fourth fifth guessedWord =
    let
        extractChar guess =
            case guess.char of
                Nothing ->
                    '_'

                Just c ->
                    case guess.status of
                        Unknown ->
                            '_'

                        Incorrect ->
                            '_'

                        _ ->
                            c
    in
    guessedWord
        |> updateCharInGuessedWord (extractChar first) 0
        |> updateCharInGuessedWord (extractChar second) 1
        |> updateCharInGuessedWord (extractChar third) 2
        |> updateCharInGuessedWord (extractChar fourth) 3
        |> updateCharInGuessedWord (extractChar fifth) 4


updateCharInGuessedWord : Char -> Int -> List Char -> List Char
updateCharInGuessedWord char position guessedWord =
    case List.drop position guessedWord |> List.head of
        Nothing ->
            guessedWord

        Just '_' ->
            {-
               if position = 0 (first char)
               [] + [x] ++ [2, 3, 4, 5]

               if position = 1 (second char)
               [1] + [x] + [3, 4, 5]

               if position = 4 (last char)
               [1, 2, 3, 4] + [x] + []
            -}
            List.take position guessedWord ++ [ char ] ++ List.drop (position + 1) guessedWord

        _ ->
            guessedWord


{-| Move the cursor to the next open letter position
-}
findNextUnlockedPosition : List Guess -> Maybe Position -> Maybe Position
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
        , button
            [ onClick SubmittedWord
            , type_ "button"
            , class
                ("px-3 py-2 rounded-sm mb-4"
                    ++ (if allLettersSet model.firstLetter model.secondLetter model.thirdLetter model.fourthLetter model.fifthLetter then
                            " bg-slate-300"

                        else
                            " bg-slate-100 cursor-not-allowed text-slate-400"
                       )
                )
            ]
            [ text "Submit" ]
        , div
            [ class "text-sm" ]
            [ p
                []
                [ text ("Remaining words: " ++ (List.length model.activeWords |> toString) ++ " of " ++ (List.length Words.wordList |> toString)) ]
            ]

        -- , div
        --     [ class "mt-4" ]
        --     [ ol
        --         []
        --         (List.map (\i -> li [] [ text i ]) model.activeWords)
        --     ]
        , div
            []
            (List.map
                (\i -> viewWordsByChar model.activeWords i)
                [ 'a'
                , 'b'
                , 'c'
                , 'd'
                , 'e'
                , 'f'
                , 'g'
                , 'h'
                , 'i'
                , 'j'
                , 'k'
                , 'l'
                , 'm'
                , 'n'
                , 'o'
                , 'p'
                , 'q'
                , 'r'
                , 's'
                , 't'
                , 'u'
                , 'v'
                , 'w'
                , 'x'
                , 'y'
                , 'z'
                ]
            )
        ]


viewPossibleLetters : List Char -> Html Msg
viewPossibleLetters letters =
    span
        []
        (List.map (\l -> span [ class "mr-1" ] [ text <| String.toUpper <| String.fromChar l ]) letters)


{-| Letter box view and styling logic
-}
viewLetterBox : Maybe Position -> Guess -> Html Msg
viewLetterBox maybePosition guess =
    let
        isSelected =
            case maybePosition of
                Nothing ->
                    False

                Just position ->
                    position == guess.position

        selectedStyle =
            case maybePosition of
                Nothing ->
                    ""

                Just position ->
                    if position == guess.position then
                        case guess.status of
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
            case guess.status of
                Unknown ->
                    " border-8 border-slate-300"

                Incorrect ->
                    " bg-slate-500 border-slate-300 text-white"

                Correct ->
                    " bg-green-500 text-white"

                Locked ->
                    " bg-green-500 border-0 text-white"

        cursorStyle =
            if guess.status == Locked then
                " cursor-not-allowed"

            else if isSelected && guess.char /= Nothing then
                " cursor-pointer"

            else
                " cursor-text"
    in
    div
        [ class ("p-4 text-center w-24 h-24 flex items-center justify-center" ++ statusStyle ++ selectedStyle ++ cursorStyle)
        , if isSelected then
            onClick <|
                case guess.char of
                    Nothing ->
                        NoOp

                    Just _ ->
                        ChangedStatus guess

          else
            onClick <| SelectedLetter guess
        ]
        [ p
            [ class "mt-0 pt-0 text-7xl leading-none" ]
            [ text <|
                case guess.char of
                    Nothing ->
                        ""

                    Just c ->
                        String.fromChar c
                            |> String.toUpper
            ]
        ]


viewWordsByChar : List String -> Char -> Html Msg
viewWordsByChar words char =
    let
        filteredWords =
            List.filter
                (\str -> String.startsWith (String.fromChar char) str)
                words
    in
    if List.isEmpty filteredWords then
        text ""

    else
        div
            [ class "mt-4" ]
            [ h2
                [ class "text-lg font-bold mt-2" ]
                [ text <| String.toUpper <| String.fromChar char ]
            , ol
                []
                (List.map (\i -> li [] [ text i ]) filteredWords)
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
