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
    300


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
    { screen : Dimension
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
    ( { screen = Dimension 0 0
      , spiderCenter = Point 0 0
      , legLength = defaultLegLength
      , points = randomPoints
      }
    , Task.perform Resize Resize Window.size
    )


randomPoints : List Point
randomPoints =
    let
        ( points, _ ) =
            step (list pointCount randomPoint) (initialSeed seed)
    in
        points


randomPoint : Generator Point
randomPoint =
    -- points are normalized between 0 and 1 --> scaled to screen upon rendering
    Random.map (\( x, y ) -> Point x y) (pair (float 0 1) (float 0 1))



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

                screen =
                    Dimension w h
            in
                ( { model
                    | screen = screen
                    , spiderCenter = Point (w / 2) (h / 2)
                  }
                , Cmd.none
                )

        KeyDown keyCode ->
            ( keyDown keyCode model
            , Cmd.none
            )

        MousePosition mousePosition ->
            ( { model | spiderCenter = mousePosition }
            , Cmd.none
            )


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    let
        w =
            model.screen.width

        h =
            model.screen.height

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
            model.screen.width

        h =
            model.screen.height

        points =
            List.map (scaleToScreen model.screen) model.points
    in
        svg
            [ viewBox (concat [ "0 0 ", toString w, " ", toString h ])
            , width "100%"
            , height "100%"
              -- prevent scrollbar
            , style "display: block"
            ]
            [ viewPoints points
            , viewSpiderCenter model.spiderCenter
            , viewSpiderLegs model.spiderCenter model.legLength points
            ]


scaleToScreen : Dimension -> Point -> Point
scaleToScreen screen point =
    Point (point.x * screen.width) (point.y * screen.height)


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
