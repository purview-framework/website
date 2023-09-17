{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Prelude hiding (div, span)
import Text.RawString.QQ (r, rQ)
import Data.Typeable

import Purview
import Code (code)
import Pages.Home as Home
import Pages.Docs as Docs

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
  , link "/docs" "Docs"
  , link "/examples" "Examples"
  , link "https://github.com/purview-framework/purview" "Github"
  ]

topLevel location = case location of
  "/" -> div
    [ nav
    , Home.component
    ]
  "/docs" -> div
    [ nav
    , Docs.component
    ]
  _ -> div [ text "Page not found" ]


data RouterEvents = SetLocation String

router :: String -> Purview () m
router initialLocation = handler' [] initialLocation reducer topLevel
  where
    reducer (SetLocation "/docs") _ = ("/docs", [])
    reducer (SetLocation "/"   )  _ = ("/",    [])

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

main :: IO ()
main = serve defaultConfiguration { htmlHead=htmlHeadAdditions, port=8001 } router
