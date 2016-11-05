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
import Time exposing (..)


-- MAIN


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = always <| subscriptions
        , view = view
        }



-- CONFIG


hexOrange : String
hexOrange =
    "#F0AD00"


hexBlue : String
hexBlue =
    "#60B5CC"


pointCount : Int
pointCount =
    300


pointRadius : Float
pointRadius =
    10


pixelsPerArrowPress : Int
pixelsPerArrowPress =
    10


initialLegLength : Float
initialLegLength =
    100


legWidth : Float
legWidth =
    5


legGrowthFactor : Float
legGrowthFactor =
    1.2


foodPointCount : Int
foodPointCount =
    10


foodPointRadius : Float
foodPointRadius =
    15



-- MSG


type Msg
    = Resize Size
    | KeyDown KeyCode
    | MousePosition Point
    | RandomPoints (List Point)
    | RandomFoodPoints (List Point)
    | DetectCollisions



-- MODEL


type alias Model =
    { screen : Dimension
    , spiderCenter : Point
    , legLength : Float
    , points : List Point
    , foodPoints : List Point
    }


type alias Point =
    { x : Float, y : Float }


type alias Dimension =
    { width : Float, height : Float }


init : ( Model, Cmd Msg )
init =
    ( { screen = Dimension 0 0
      , spiderCenter = Point 0 0
      , legLength = initialLegLength
      , points = []
      , foodPoints = []
      }
    , Cmd.batch
        [ Task.perform Resize Resize Window.size
        , Random.generate RandomPoints (list pointCount randomPoint)
        , Random.generate RandomFoodPoints (list foodPointCount randomPoint)
        ]
    )


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
        RandomPoints points ->
            ( { model | points = points }
            , Cmd.none
            )

        RandomFoodPoints foodPoints ->
            ( { model | foodPoints = foodPoints }
            , detectCollisions
            )

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
                , detectCollisions
                )

        KeyDown keyCode ->
            ( keyDown keyCode model
            , detectCollisions
            )

        MousePosition mousePosition ->
            ( { model | spiderCenter = mousePosition }
            , detectCollisions
            )

        DetectCollisions ->
            let
                notEaten =
                    (\p -> not (collidingWithSpider model.spiderCenter p model.screen))

                remainingFoodPoints =
                    List.filter notEaten model.foodPoints

                eatenPointCount =
                    (List.length model.foodPoints) - (List.length remainingFoodPoints)

                grownLegLength =
                    (legGrowthFactor ^ (toFloat eatenPointCount)) * model.legLength
            in
                ( { model
                    | foodPoints = remainingFoodPoints
                    , legLength = grownLegLength
                  }
                , Cmd.none
                )


detectCollisions : Cmd Msg
detectCollisions =
    Task.perform identity identity (Task.succeed DetectCollisions)


collidingWithSpider : Point -> Point -> Dimension -> Bool
collidingWithSpider spiderCenter point screen =
    let
        ( x1, y1 ) =
            ( spiderCenter.x, spiderCenter.y )

        foodPoint =
            scaleToScreen screen point

        ( x2, y2 ) =
            ( foodPoint.x, foodPoint.y )

        a =
            (x1 - x2)

        b =
            (y1 - y2)

        c =
            pointRadius + foodPointRadius
    in
        a * a + b * b < c * c


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

        foodPoints =
            List.map (scaleToScreen model.screen) model.foodPoints
    in
        svg
            [ viewBox (concat [ "0 0 ", toString w, " ", toString h ])
            , width "100%"
            , height "100%"
              -- prevent scrollbar
            , style "display: block"
            ]
            [ viewPoints points
            , viewFoodPoints foodPoints
            , viewSpiderCenter model.spiderCenter
            , viewSpiderLegs model.spiderCenter model.legLength points
            ]


scaleToScreen : Dimension -> Point -> Point
scaleToScreen screen point =
    Point (point.x * screen.width) (point.y * screen.height)


viewSpiderCenter : Point -> Svg msg
viewSpiderCenter spiderCenter =
    viewPoint spiderCenter hexOrange pointRadius


viewPoints : List Point -> Svg msg
viewPoints points =
    g [] (List.map (\p -> viewPoint p "black" pointRadius) points)


viewFoodPoints : List Point -> Svg msg
viewFoodPoints points =
    g [] (List.map (\p -> viewPoint p hexBlue foodPointRadius) points)


viewPoint : Point -> String -> Float -> Svg msg
viewPoint point color radius =
    circle
        [ cx (toString point.x)
        , cy (toString point.y)
        , r (toString radius)
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
        , stroke hexOrange
        , strokeWidth (toString legWidth)
        , strokeLinecap "round"
        ]
        []
