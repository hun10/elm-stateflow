module Flow where

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard
import String

main =
  Signal.map (view 0.5) paragraphs

view opacity paragraphs =
  div [ style [ ("padding", "20px")
              , ("font-family", "Modern")
              , ("font-size", "14pt")
              , ("opacity", toString opacity)
              ]]
  <| List.map (\txt -> p [] [text txt]) paragraphs

paragraphs =
  Signal.foldp update [] actions

update action model =
  let
    reversed = List.reverse model
  in
  case (action, reversed) of
    (Typed s, []) ->
      [s]
    
    (NewParagraph, []) ->
      []
    
    (Typed s, last :: previous) ->
      List.reverse <| (last ++ s) :: previous
    
    (NewParagraph, "" :: previous) ->
      model
    
    (NewParagraph, _) ->
      model ++ []

type Action
  = Typed String
  | NewParagraph

actions =
  Signal.merge newParagraphs chars

newParagraphs =
  Signal.map (\_ -> NewParagraph) Keyboard.enter

chars =
  Signal.map codeToAction Keyboard.presses

codeToAction code =
  Typed (String.fromChar << Char.fromCode <| code)
