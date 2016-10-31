module Main exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (toHtml)
import Html exposing (Html)
import Html.App exposing (program)
import Platform.Cmd
import Platform.Sub
import Task exposing (perform)
import Window exposing (Size)
import Keyboard exposing (..)
import Debug exposing (..)
import Text exposing (..)
import Random exposing (..)


-- CONFIG


seed : Int
seed =
    42


pointCount : Int
pointCount =
    100


pointRadius : Float
pointRadius =
    10


pixelsPerArrowPress : Int
pixelsPerArrowPress =
    10



-- MSG


type Msg
    = Resize Size
    | KeyDown KeyCode



-- MODEL


type alias Model =
    { width : Int
    , height : Int
    , x : Int
    , y : Int
    , points : List ( Float, Float )
    }


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = always <| subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( { width = 0
      , height = 0
      , x = 0
      , y = 0
      , points = []
      }
    , Task.perform Resize Resize Window.size
    )



-- SUB


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Window.resizes Resize
        , Keyboard.downs KeyDown
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize s ->
            ( { model
                | width = s.width
                , height = s.height
                , points = List.map (scalePoint s.width s.height) randomPoints
              }
            , Cmd.none
            )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )


scalePoint : Int -> Int -> ( Float, Float ) -> ( Float, Float )
scalePoint width height ( x, y ) =
    let
        w =
            toFloat width

        h =
            toFloat height
    in
        ( x * w - (w / 2), y * h - (h / 2) )


randomPoints : List ( Float, Float )
randomPoints =
    let
        ( points, _ ) =
            step (list pointCount randomPoint) (initialSeed seed)
    in
        points


randomPoint : Generator ( Float, Float )
randomPoint =
    pair (float 0 1) (float 0 1)


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    let
        halfWidth =
            round (toFloat model.width / 2)

        halfHeight =
            round (toFloat model.height / 2)
    in
        case keyCode of
            -- arrow left
            37 ->
                { model | x = max (model.x - pixelsPerArrowPress) -halfWidth }

            -- arrow up
            38 ->
                { model | y = min (model.y + pixelsPerArrowPress) halfHeight }

            -- arrow right
            39 ->
                { model | x = min (model.x + pixelsPerArrowPress) halfWidth }

            -- arrow down
            40 ->
                { model | y = max (model.y - pixelsPerArrowPress) -halfHeight }

            _ ->
                model



-- VIEW


view : Model -> Html Msg
view model =
    toHtml <|
        collage
            model.width
            model.height
            [ filled black <| rect (toFloat model.width) (toFloat model.height)
            , move ( toFloat model.x, toFloat model.y ) <| filled orange <| circle pointRadius
            , viewPoints model.width model.height model.points
            ]


viewPoints : Int -> Int -> List ( Float, Float ) -> Form
viewPoints width height points =
    group (List.map viewPoint points)


viewPoint : ( Float, Float ) -> Form
viewPoint ( x, y ) =
    move ( x, y ) <| filled white <| circle pointRadius
