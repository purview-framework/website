{-# LANGUAGE QuasiQuotes #-}
module Main where

import Prelude hiding (div, span)
import Text.RawString.QQ (r)

import Purview

navStyle = [style|
  width: 100%;
  min-height: 50px;
  padding-top: 25px;
  display: flex;
  justify-content: space-around;
|]

aStyle = [style|
  text-decoration: none;
|]

link location str = aStyle $ href location $ a [ text str ]

headlineStyle = [style|
  font-style: normal;
  color: #b76e79;
|]

titleStyle = [style|
  font-family: "Sagittarius A", "Sagittarius B";
  font-size: 100px;
  font-weight: 400;
  margin: 1rem;
  letter-spacing: 5px;
|]

alternateColors :: String -> [Purview event m]
alternateColors str =
  let
    letterAndColor = zip str (cycle ["#c1db75", "#eebd53"])
  in
    fmap
      (\(letter, color) -> istyle ("color: " <> color) $ span [ text [letter] ] )
      letterAndColor

title str
  = titleStyle
  $ headlineStyle
  $ h1 str

subtitleStyle = [style|
  font-size: 24px;
  margin: 1rem;
  font-weight: 400;
|]

subtitle str
  = subtitleStyle
  $ headlineStyle
  $ h2 [ text str ]

nav = navStyle $ div
  [ link "/" "Home"
  , link "/docs" "Docs"
  , link "/examples" "Examples"
  , link "/github" "Github"
  ]

topBoxStyle = [style|
  text-align: center;
  padding-top: 10%;
|]

dividerStyle = [style|
  border: 3px solid #b76e79;
  background-color: #b76e79;
  max-width: 15px;
  height: 15px;
  width: 15px;
  border-radius: 25px;
  margin: auto;
  margin-bottom: 45px;
|]


topLevel = div
  [ nav
  , topBoxStyle $ div
      [ title $ alternateColors "Purview"
      -- , dividerStyle $ div []
      , subtitle "Build server rendered, interactive websites with Haskell"
      ]
  ]

router _ = topLevel

htmlHeadAdditions = [r|
  <style>
    body {
      margin: 0;
      padding: 0;
      font-family: "Gotham SSm A", "Gotham SSm B", sans-serif;
      font-size: 18px;
      max-width: 65rem;
      margin: auto;
    }
  </style>
  <link rel="stylesheet" type="text/css" href="https://cloud.typography.com/6107252/6057832/css/fonts.css" />
|]

main :: IO ()
main = serve defaultConfiguration { htmlHead=htmlHeadAdditions } router
