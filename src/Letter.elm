module Letter exposing (Letter, Status(..), initLetters)


type alias Letter =
    { char : Char
    , status : Status
    }


type Status
    = Unknown
    | Incorrect
    | Correct
    | Locked


initLetters : List Letter
initLetters =
    [ Letter 'a' Unknown
    , Letter 'b' Unknown
    , Letter 'c' Unknown
    , Letter 'd' Unknown
    , Letter 'e' Unknown
    , Letter 'f' Unknown
    , Letter 'g' Unknown
    , Letter 'h' Unknown
    , Letter 'i' Unknown
    , Letter 'j' Unknown
    , Letter 'k' Unknown
    , Letter 'l' Unknown
    , Letter 'm' Unknown
    , Letter 'n' Unknown
    , Letter 'o' Unknown
    , Letter 'p' Unknown
    , Letter 'q' Unknown
    , Letter 'r' Unknown
    , Letter 's' Unknown
    , Letter 't' Unknown
    , Letter 'u' Unknown
    , Letter 'v' Unknown
    , Letter 'w' Unknown
    , Letter 'x' Unknown
    , Letter 'y' Unknown
    , Letter 'z' Unknown
    ]
