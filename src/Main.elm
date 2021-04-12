module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, span, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Svg exposing (circle, line, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, viewBox, width, x1, x2, y1, y2)
import Task
import Time exposing (Posix, Zone, ZoneName)



-- MODEL


type alias Model =
    { time : Maybe Posix
    , zone : Maybe Zone
    , zoneName : Maybe ZoneName
    , paused : Bool
    }


type Msg
    = AdjustTimeZone Zone
    | UpdateZoneName ZoneName
    | Tick Posix
    | Pause
    | Resume



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Nothing Nothing False
    , Task.perform AdjustTimeZone Time.here
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Time"
    , body = [ viewTime model ]
    }


viewTime : Model -> Html Msg
viewTime model =
    case model.time of
        Just time ->
            case model.zone of
                Just zone ->
                    case model.zoneName of
                        Just zoneName ->
                            div []
                                [ div [ style "display" "flex", style "justify-content" "space-around", style "align-items" "center" ]
                                    [ viewPosix time zone zoneName
                                    , viewClock time zone
                                    , viewPause model
                                    ]
                                , div [ style "display" "flex", style "justify-content" "center" ] [ a [ href "https://github.com/csaltos/elm-clock/blob/main/src/Main.elm" ] [ text "https://github.com/csaltos/elm-clock" ] ]
                                ]

                        Nothing ->
                            text ""

                Nothing ->
                    text ""

        Nothing ->
            text ""


viewPause : Model -> Html Msg
viewPause model =
    if model.paused then
        button [ onClick Resume ] [ text "Resume" ]

    else
        button [ onClick Pause ] [ text "Pause" ]


toStringTimeString : Int -> String
toStringTimeString time =
    if time < 10 then
        "0" ++ String.fromInt time

    else
        String.fromInt time


viewPosix : Posix -> Zone -> ZoneName -> Html Msg
viewPosix time zone zoneName =
    let
        hour =
            toStringTimeString (Time.toHour zone time)

        minute =
            toStringTimeString (Time.toMinute zone time)

        second =
            toStringTimeString (Time.toSecond zone time)

        z =
            case zoneName of
                Time.Name name ->
                    name

                Time.Offset offset ->
                    String.fromInt offset
    in
    span [ style "font-family" "Monospace" ] [ text (hour ++ ":" ++ minute ++ ":" ++ second ++ " " ++ z) ]


viewClock : Posix -> Zone -> Html Msg
viewClock time zone =
    let
        angleHour =
            turns (toFloat (modBy 12 (Time.toHour zone time)) / 12.0) - turns (1 / 4)

        hourX =
            String.fromFloat (60 + 20 * cos angleHour)

        hourY =
            String.fromFloat (60 + 20 * sin angleHour)

        angleMinute =
            turns (toFloat (Time.toMinute zone time) / 60.0) - turns (1 / 4)

        minuteX =
            String.fromFloat (60 + 35 * cos angleMinute)

        minuteY =
            String.fromFloat (60 + 35 * sin angleMinute)

        angleSecond =
            turns (toFloat (Time.toSecond zone time) / 60.0) - turns (1 / 4)

        secondX =
            String.fromFloat (60 + 40 * cos angleSecond)

        secondY =
            String.fromFloat (60 + 40 * sin angleSecond)
    in
    svg [ viewBox "0 0 120 120", width "120", height "120" ]
        [ circle [ cx "60", cy "60", r "50", fill "blue" ] []
        , line [ x1 "60", y1 "60", x2 hourX, y2 hourY, stroke "white" ] []
        , line [ x1 "60", y1 "60", x2 minuteX, y2 minuteY, stroke "white" ] []
        , line [ x1 "60", y1 "60", x2 secondX, y2 secondY, stroke "white" ] []
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | zone = Just zone }, Task.perform UpdateZoneName Time.getZoneName )

        UpdateZoneName zoneName ->
            ( { model | zoneName = Just zoneName }, Cmd.none )

        Tick time ->
            ( { model | time = Just time }, Cmd.none )

        Pause ->
            ( { model | paused = True }, Cmd.none )

        Resume ->
            ( { model | paused = False }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Time.every 500 Tick
