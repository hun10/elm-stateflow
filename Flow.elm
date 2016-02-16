module Flow where

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard
import String

main =
  Signal.map (view 0.5) chars

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

chars =
  Signal.map (String.fromChar << Char.fromCode) Keyboard.presses
