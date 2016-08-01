module Video.Video exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Time exposing(Time)
import Task exposing (Task)
import Debug

import Slider.Slider as Slider


type State
  = Pause
  | Playing

type Direction
  = Prev
  | Next

type alias Model =
  { slider : Slider.Model
  --, beginAt : Time
  --, during : Time
  , state : State
  , speed : Int
  --, fps : Float
  --, frames : List a
  --, curr : Int
  }

initModel : Model
initModel =
  { slider = Slider.initModel
  , state  = Pause
  , speed  = 8
  }

type Msg
  = NoOp
  | MsgSlider Slider.Msg
  | ToggleState
  | ToggleDoneState Time
  | JumpFrame (Maybe Int) (Maybe Direction)
  | Tick Time



init : (Model, Cmd Msg)
init =
  let
    (mSlider, fxSlider) =
      Slider.init
    fx =
      Cmd.batch
        [ Cmd.map MsgSlider fxSlider
        ]
  in
    (initModel, fx)

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ state } as model) =
  case msg of
    MsgSlider msgSlider ->
      let
        (m, fx) =
          Slider.update msgSlider model.slider
      in
        ( { model | slider = m }
        , Cmd.map MsgSlider fx
        )

    ToggleState ->
      let
        turnState =
          case state of
            Pause ->
              Playing
            Playing ->
              Pause

        fx =
          Task.perform (\_ -> NoOp) ToggleDoneState Time.now
      in
        ( { model | state = turnState }
        , fx
        )
    ToggleDoneState t ->
      let
        _ =
          Debug.log "time" t
      in
        (model, Cmd.none)
    Tick t ->
      let
        _ =
          Debug.log "tt" t
      in
        (model, Cmd.none)
    _ ->
      (model, Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    fxTimer =
      case model.state of
        Pause ->
          Sub.none
        Playing ->
          Time.every (Time.second / (toFloat model.speed)) Tick
  in
    Sub.batch
       [ Sub.map MsgSlider (Slider.subscriptions model.slider)
       , fxTimer
       ]



-- VIEW

view : Model -> Html Msg
view ({ state } as model) =
  let
    playStateView =
      case state of
        Pause ->
          text "||"
        Playing ->
          text "|>"

    controlView =
      div [ class "video-control row" ]
        [ div [ class "video-btn-group grid width--4" ]
            [ button [ class "video-btn" ] [ text "<<" ]
            , button [ class "video-btn video-btn--lg"
                     , onClick ToggleState
                     ]
                [ playStateView ]
            , button [ class "video-btn" ] [ text ">>" ]
            ]
        , div [ class "video-slider grid width--8" ]
            [ App.map MsgSlider <| Slider.view model.slider
            ]
        ]

    screenView =
      div [ class "video-screen"]
        [
        ]

    view' =
      div [ class "video" ]
        [ screenView
        , controlView
        ]

  in
    view'
