module Flow where

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import String
import Time exposing (Time)

main =
  Signal.map view state

defaultTimeToFade = 5 * Time.second

ease x =
  1 - (1 - x) ^ 1.5

view m =
  case m.state of
    ChooseSessionTime ->
      div []
        <| List.map (buttonForSessionTime newSessionTimes.address)
        <| List.map (\x -> x * Time.minute) [5, 10, 15, 20, 25, 30, 45, 60]
    
    SessionEnded ->
      div []
      [ viewSession m
      , button [onClick startOvers.address StartOver] [text "Start Over"]
      ]
    
    _ ->
      viewSession m

buttonForSessionTime address time =
  button [onClick address time]
    [viewTime time]

viewSession {model, timeToFade, sessionTimeLeft} =
  div []
  [ viewTime sessionTimeLeft
  , viewText model timeToFade
  ]

viewText model timeToFade =
  div [ style [ ("padding", "20px")
              , ("font-family", "Modern")
              , ("font-size", "14pt")
              , ("opacity", toString << ease  <| timeToFade / defaultTimeToFade )
              ]]
  <| List.map (\txt -> p [] [text txt]) <| model

viewTime t =
  let
    seconds = floor (t / Time.second) % 60
    minutes = floor (t / Time.minute)
    dd = String.padLeft 2 '0' << toString
  in
    text <| (dd minutes) ++ ":" ++ (dd seconds)

init = { model = []
       , timeToFade = defaultTimeToFade
       , sessionTimeLeft = 0
       , state = ChooseSessionTime
       }

state =
  Signal.foldp update init actions

update action m =
  let
    reversed = List.reverse model
    model = m.model
    timeToFade = m.timeToFade
  in
  case (m.state, action) of
    (WaitingForFirstStroke, Typed _) ->
      update action { m | state = SessionInProgress }
    
    (SessionInProgress, _) ->
      case (action, reversed) of
        (Typed s, []) ->
          { m | model = [s]
              , timeToFade = defaultTimeToFade
          }
        
        (NewParagraph, []) ->
          { m | model = []
              , timeToFade = defaultTimeToFade
          }
        
        (Typed s, last :: previous) ->
          { m | model = List.reverse <| (last ++ s) :: previous
              , timeToFade = defaultTimeToFade
          }
        
        (NewParagraph, "" :: previous) ->
          m
        
        (NewParagraph, _) ->
          { m | model = model ++ [""]
              , timeToFade = defaultTimeToFade
          }
        
        (TimeElapsed t, _) ->
          if m.sessionTimeLeft > t then
            if t > timeToFade then
              { m | model = []
                  , timeToFade = 0
                  , sessionTimeLeft = m.sessionTimeLeft - t
              }
            else
              { m | timeToFade = timeToFade - t
                  , sessionTimeLeft = m.sessionTimeLeft - t
              }
          else
            { m | timeToFade = defaultTimeToFade
                , sessionTimeLeft = 0
                , state = SessionEnded
            }
        
        _ -> -- TODO: make nesting (or actions translations) to expell all disallowed action
          m
    
    (ChooseSessionTime, SetSession t) ->
      { m | sessionTimeLeft = t
          , state = WaitingForFirstStroke
      }
    
    (SessionEnded, StartOver) ->
      init
    
    _ ->
      m

type States
  = WaitingForFirstStroke
  | ChooseSessionTime
  | SessionInProgress
  | SessionEnded

type Action
  = Typed String
  | NewParagraph
  | TimeElapsed Time
  | SetSession Time
  | StartOver
  | Nothing

newSessionTimes : Signal.Mailbox Time
newSessionTimes =
  Signal.mailbox 0

startOvers : Signal.Mailbox Action
startOvers =
  Signal.mailbox Nothing

actions =
  Signal.mergeMany [ newParagraphs
                   , spaceBars
                   , chars
                   , ticks
                   , Signal.map SetSession newSessionTimes.signal
                   , startOvers.signal
                   ]

ticks =
  Signal.map TimeElapsed (Time.fps 5)

spaceBars =
  Signal.map (\_ -> Typed " ") Keyboard.space

newParagraphs =
  Signal.map (\_ -> NewParagraph) Keyboard.enter

chars =
  Signal.map codeToAction Keyboard.presses

codeToAction code =
  Typed (String.trim << String.fromChar << Char.fromCode <| code)
