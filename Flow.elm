module Flow where

import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard

main = Signal.map (text << toString) Keyboard.presses
--  view 0.5 "Hello helloh helloh helo русские буквы как смотрят"

view opacity txt =
  div [ style [ ("padding", "20px")
              , ("margin", "40px")
              , ("border", "1px solid")
              , ("font-family", "Modern")
              , ("font-size", "14pt")
              , ("opacity", toString opacity)
              ]]
  [ text txt
  ]
