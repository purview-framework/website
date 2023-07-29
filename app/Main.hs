{-# LANGUAGE QuasiQuotes #-}
module Main where

import Prelude hiding (div)
import Text.RawString.QQ (r)

import Purview

navStyle = [style|
  width: 100%;
  min-height: 50px;
  padding-top: 25px;
|]

aStyle = [style|
  padding: 20px;
  text-decoration: none;
|]

link location str = aStyle $ href location $ a [ text str ]

headlineStyle = [style|
  font-family: "Sagittarius A", "Sagittarius B";
  font-style: normal;
  font-weight: 400;
  color: #A5BF04;
|]

titleStyle = [style|
  font-size: 96px;
  margin: 1rem;
  letter-spacing: 5px;
|]

title str
  = titleStyle
  $ headlineStyle
  $ h1 [ text str ]

subtitleStyle = [style|
  font-size: 36px;
  margin: 1rem;
|]

subtitle str
  = subtitleStyle
  $ headlineStyle
  $ h2 [ text str ]

nav = navStyle $ div
  [ link "/" "Home"
  , link "/examples" "Examples"
  , link "/docs" "Docs"
  ]

centerStyle = [style|
  text-align: center;
|]

topLevel = div
  [ nav
  , centerStyle $ div
      [ title "Purview"
      , subtitle "Build server rendered interactive websites with Haskell"
      ]
  ]

router _ = topLevel

htmlHeadAdditions = [r|
  <style>
    body {
      margin: 0;
      padding: 0;
      font-family: sans-serif;
      font-size: 18px;
    }
  </style>
  <link rel="stylesheet" type="text/css" href="https://cloud.typography.com/6107252/6057832/css/fonts.css" />
|]

main :: IO ()
main = serve defaultConfiguration { htmlHead=htmlHeadAdditions } router
