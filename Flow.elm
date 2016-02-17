module Flow where

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard
import String

main =
  Signal.map (view 0.5) typedText

view opacity txt =
  div [ style [ ("padding", "20px")
              , ("font-family", "Modern")
              , ("font-size", "14pt")
              , ("opacity", toString opacity)
              ]]
  [ text txt
  ]

typedText =
  Signal.foldp (flip (++)) "" chars

chars =
  Signal.map (String.fromChar << Char.fromCode) Keyboard.presses
