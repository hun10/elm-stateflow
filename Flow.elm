module Flow where

import Html exposing (..)
import Html.Attributes exposing (..)

main =
  view 0.5

view opacity =
  div [ style [ ("padding", "20px")
              , ("margin", "40px")
              , ("border", "1px solid")
              , ("font-family", "Modern")
              , ("font-size", "14pt")
              , ("opacity", toString opacity)
              ]]
  [ text "Hello helloh helloh helo русские буквы как смотрят"
  ]
