{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Prelude hiding (div, span)
import Text.RawString.QQ (r, rQ)
import Data.Typeable

import Purview
import Purview.Server
import Code (code)
import Events ( RouterEvents(SetLocation) )
import Pages.Home as Home
import Pages.Docs as Docs
import Pages.Examples as Examples

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

link location str =
  aStyle
  $ href location
  $ a [ text str ]

nav = navStyle $ div
  [ link "/" "Home"
  , link "/docs/intro" "Docs"
  , link "/examples/effectful-time" "Examples"
  , link "https://hackage.haskell.org/package/purview" "Hackage"
  , link "https://github.com/purview-framework/purview" "Github"
  ]

topLevelStyle = [style|
  margin-bottom: 90px;
|]

topLevel location = topLevelStyle $ case takeWhile (/= '/') (drop 1 location) of
  "" -> div
    [ nav
    , Home.component
    ]
  "docs" -> div
    [ nav
    , Docs.component location
    ]
  "examples" -> div
    [ nav
    , Examples.component location
    ]
  _ -> div []


router :: String -> Purview () m
router initialLocation = handler' [] initialLocation reducer topLevel
  where
    reducer (SetLocation "/docs") _ = ("/docs", [])
    reducer (SetLocation "/"   )  _ = ("/",    [])
    reducer (SetLocation str   )  _ = (str, [Browser "addLocation" str])

htmlHeadAdditions = [r|
  <style>
    body {
      margin: 0;
      padding: 0;
      font-family: "Gotham SSm A", "Gotham SSm B", sans-serif;
      font-size: 18px;
      max-width: 75rem;
      margin: auto;
      color: #514c39;
    }
    a {
      color: #514c39;
      text-decoration: none;
    }
  </style>
  <link rel="stylesheet" type="text/css" href="https://cloud.typography.com/6107252/6057832/css/fonts.css" />
|]

js = [r|
const addLocation = (newLocation) => {
  history.pushState({}, "", newLocation)
}
window.addLocation = addLocation;
|]

main :: IO ()
main = serve
  defaultConfiguration { htmlHead=htmlHeadAdditions, javascript=js, port=8001, secure=True }
  router
