import Html exposing (..)
import Browser exposing (sandbox)
import Parser exposing (Parser, (|.), (|=))

-- MODEL

input = """#1 @ 912,277: 27x20"""

main =
    Browser.sandbox { init = init, update = update, view = view }

type alias Model = String

init : Model
init =
    input


-- UPDATE

type Msg = Compute

update : Msg -> Model -> Model
update msg model =
    model

-- VIEW

view : Model -> Html Msg
view model =
    let
        sumText = transform model
        x = String.fromInt sumText.x
        y = String.fromInt sumText.y
        w = String.fromInt sumText.w
        h = String.fromInt sumText.h
    in
        p [] [ text (String.concat [x,y,w,h]) ]

transform : String -> Result String Rectangle
transform val =
    readRectangles input |> Maybe.withDefault {x=0,y=0,w=0,h=0}

type alias Rectangle =
    {  x : Int
    ,  y : Int
    ,  w : Int
    ,  h : Int
    }

readRectangles : String -> Result String Rectangle
readRectangles lines =
    Parser.run rectangle lines

rectangle : Parser Rectangle
rectangle =
    Parser.succeed Rectangle
    |. Parser.symbol "#"
    |. Parser.int
    |. Parser.symbol " @ "
    |= Parser.int 
    |. Parser.symbol ","
    |= Parser.int
    |. Parser.symbol ": "
    |= Parser.int
    |. Parser.symbol
    |= Parser.int
    |. Parser.end
