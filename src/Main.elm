module Main exposing (..)

import Color exposing (..)
import Html exposing (Html)
import Html.App exposing (program)
import Platform.Cmd
import Platform.Sub
import Task exposing (perform)
import Window exposing (Size)
import Keyboard exposing (..)
import Debug exposing (..)
import Random exposing (..)
import Mouse exposing (..)
import Svg exposing (line, g, circle, Svg, svg)
import Svg.Attributes exposing (..)
import String exposing (concat)


-- CONFIG


elmOrange : String
elmOrange =
    "#F0AD00"


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
                w =
                    toFloat s.width

                h =
                    toFloat s.height

                dimension =
                    Dimension w h
            in
                ( { model
                    | dimension = dimension
                    , spiderCenter = Point (w / 2) (h / 2)
                    , points = List.map (scalePoint dimension) randomPoints
                  }
                , Cmd.none
                )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        MousePosition mousePosition ->
            ( { model | spiderCenter = mousePosition }
            , Cmd.none
            )


scalePoint : Dimension -> Point -> Point
scalePoint dimension point =
    let
        x =
            point.x * dimension.width

        y =
            point.y * dimension.height
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
        w =
            model.dimension.width

        h =
            model.dimension.height

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
                    Point (Basics.max (x - delta) 0) y

                -- arrow up
                38 ->
                    Point x (Basics.max (y - delta) 0)

                -- arrow right
                39 ->
                    Point (Basics.min (x + delta) w) y

                -- arrow down
                40 ->
                    Point x (Basics.min (y + delta) h)

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
        svg
            [ viewBox (concat [ "0 0 ", toString w, " ", toString h ])
            , width "100%"
            , height "100%"
              -- prevent scrollbar
            , style "display: block"
            ]
            [ viewPoints model.points
            , viewSpiderCenter model.spiderCenter
            , viewSpiderLegs model.spiderCenter model.legLength model.points
            ]


viewSpiderCenter : Point -> Svg msg
viewSpiderCenter spiderCenter =
    viewPoint spiderCenter elmOrange


viewPoints : List Point -> Svg msg
viewPoints points =
    g [] (List.map (flip viewPoint "black") points)


viewPoint : Point -> String -> Svg msg
viewPoint point color =
    circle
        [ cx (toString point.x)
        , cy (toString point.y)
        , r (toString pointRadius)
        , fill color
        ]
        []


viewSpiderLegs : Point -> Float -> List Point -> Svg msg
viewSpiderLegs spiderCenter legLength points =
    let
        legPoints =
            List.filter (withinLegRange spiderCenter legLength) points
    in
        g [] (List.map (viewSpiderLeg spiderCenter) legPoints)


withinLegRange : Point -> Float -> Point -> Bool
withinLegRange spiderCenter legLength point =
    let
        dx =
            spiderCenter.x - point.x

        dy =
            spiderCenter.y - point.y
    in
        (dx * dx) + (dy * dy) < legLength * legLength


viewSpiderLeg : Point -> Point -> Svg msg
viewSpiderLeg spiderCenter legEnd =
    line
        [ x1 (toString spiderCenter.x)
        , y1 (toString spiderCenter.y)
        , x2 (toString legEnd.x)
        , y2 (toString legEnd.y)
        , stroke elmOrange
        , strokeWidth (toString legWidth)
        , strokeLinecap "round"
        ]
        []
