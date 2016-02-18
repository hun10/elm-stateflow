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

view {model, timeToFade} =
  div [ style [ ("padding", "20px")
              , ("font-family", "Modern")
              , ("font-size", "14pt")
              , ("opacity", toString <| timeToFade / defaultTimeToFade )
              ]]
  <| List.map (\txt -> p [] [text txt]) <| model

state =
  Signal.foldp update { model = [], timeToFade = 0 } actions

update action m =
  let
    reversed = List.reverse model
    model = m.model
    timeToFade = m.timeToFade
  in
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
        { model = []
        , timeToFade = 0
        }
      else
        { m | timeToFade = timeToFade - t
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
