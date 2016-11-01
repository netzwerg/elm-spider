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
import Mouse exposing (..)


-- CONFIG


seed : Int
seed =
    42


pointCount : Int
pointCount =
    200


pointRadius : Float
pointRadius =
    10


pixelsPerArrowPress : Int
pixelsPerArrowPress =
    10


defaultLegLength : Float
defaultLegLength =
    100


legWidth : Float
legWidth =
    5



-- MSG


type Msg
    = Resize Size
    | KeyDown KeyCode
    | MousePosition Point



-- MODEL


type alias Model =
    { dimension : Dimension
    , spiderCenter : Point
    , legLength : Float
    , points : List Point
    }


type alias Point =
    { x : Float, y : Float }


type alias Dimension =
    { width : Float, height : Float }


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
    ( { dimension = Dimension 0 0
      , spiderCenter = Point 0 0
      , legLength = defaultLegLength
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
        , Mouse.moves (\{ x, y } -> MousePosition (Point (toFloat x) (toFloat y)))
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize s ->
            let
                dimension =
                    Dimension (toFloat s.width) (toFloat s.height)
            in
                ( { model
                    | dimension = dimension
                    , points = List.map (scalePoint dimension) randomPoints
                  }
                , Cmd.none
                )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        MousePosition mousePosition ->
            let
                x =
                    mousePosition.x - model.dimension.width / 2

                y =
                    model.dimension.height / 2 - mousePosition.y
            in
                ( { model | spiderCenter = Point x y }
                , Cmd.none
                )


scalePoint : Dimension -> Point -> Point
scalePoint dimension point =
    let
        x =
            point.x * dimension.width - (dimension.width / 2)

        y =
            point.y * dimension.height - (dimension.height / 2)
    in
        Point x y


randomPoints : List Point
randomPoints =
    let
        ( points, _ ) =
            step (list pointCount randomPoint) (initialSeed seed)
    in
        points


randomPoint : Generator Point
randomPoint =
    Random.map (\( x, y ) -> Point x y) (pair (float 0 1) (float 0 1))


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    let
        halfWidth =
            model.dimension.width / 2

        halfHeight =
            model.dimension.height / 2

        x =
            model.spiderCenter.x

        y =
            model.spiderCenter.y

        delta =
            toFloat pixelsPerArrowPress

        spiderCenter =
            case keyCode of
                -- arrow left
                37 ->
                    Point (max (x - delta) -halfWidth) y

                -- arrow up
                38 ->
                    Point x (min (y + delta) halfHeight)

                -- arrow right
                39 ->
                    Point (min (x + delta) halfWidth) y

                -- arrow down
                40 ->
                    Point x (max (y - delta) -halfHeight)

                _ ->
                    Point x y
    in
        { model | spiderCenter = spiderCenter }



-- VIEW


view : Model -> Html Msg
view model =
    let
        w =
            model.dimension.width

        h =
            model.dimension.height
    in
        toHtml <|
            collage
                (round w)
                (round h)
                [ filled black <| rect w h
                , viewPoints model.points
                , viewSpiderCenter model.spiderCenter
                , viewSpiderLegs model.spiderCenter model.legLength model.points
                ]


viewSpiderCenter : Point -> Form
viewSpiderCenter spiderCenter =
    move ( spiderCenter.x, spiderCenter.y ) <| filled orange <| circle pointRadius


viewPoints : List Point -> Form
viewPoints points =
    group (List.map viewPoint points)


viewPoint : Point -> Form
viewPoint point =
    move ( point.x, point.y ) <| filled white <| circle pointRadius


viewSpiderLegs : Point -> Float -> List Point -> Form
viewSpiderLegs spiderCenter legLength points =
    let
        legPoints =
            List.filter (withinLegRange spiderCenter legLength) points
    in
        group (List.map (viewSpiderLeg spiderCenter) legPoints)


withinLegRange : Point -> Float -> Point -> Bool
withinLegRange spiderCenter legLength point =
    let
        dx =
            spiderCenter.x - point.x

        dy =
            spiderCenter.y - point.y
    in
        (dx * dx) + (dy * dy) < legLength * legLength


viewSpiderLeg : Point -> Point -> Form
viewSpiderLeg spiderCenter legEnd =
    let
        lineStyle =
            { defaultLine
                | width = legWidth
                , color = orange
                , cap = Round
            }
    in
        segment ( spiderCenter.x, spiderCenter.y ) ( legEnd.x, legEnd.y ) |> traced lineStyle
