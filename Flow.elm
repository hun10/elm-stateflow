module Flow where

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard
import String
import Time exposing (Time)

main =
  Signal.map view state

defaultTimeToFade = 5 * Time.second

view {model, timeToFade, sessionTimeLeft} =
  div [ style [ ("padding", "20px")
              , ("font-family", "Modern")
              , ("font-size", "14pt")
              , ("opacity", toString <| timeToFade / defaultTimeToFade )
              ]]
  <| (viewTime sessionTimeLeft) :: (List.map (\txt -> p [] [text txt]) <| model)

viewTime t =
  let
    seconds = floor (t / Time.second) % 60
    minutes = floor (t / Time.minute)
    dd = String.padLeft 2 '0' << toString
  in
    text <| (dd minutes) ++ ":" ++ (dd seconds)

state =
  Signal.foldp update { model = []
                      , timeToFade = defaultTimeToFade
                      , sessionTimeLeft = 5 * Time.minute
                      } actions

update action m =
  let
    reversed = List.reverse model
    model = m.model
    timeToFade = m.timeToFade
  in
  if m.sessionTimeLeft > 0 then
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
    }

type Action
  = Typed String
  | NewParagraph
  | TimeElapsed Time

actions =
  Signal.mergeMany [newParagraphs, spaceBars, chars, ticks]

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
