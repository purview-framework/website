{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Prelude hiding (div, span)
import Text.RawString.QQ (r)
import Data.Typeable

import Purview
import Code (code)

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

headlineStyle = [style|
  font-style: normal;
|]

titleStyle = [style|
  font-family: "Sagittarius A", "Sagittarius B";
  font-size: 124px;
  font-weight: 400;
  margin: 0 auto;
  padding: 0;
  letter-spacing: 5px;
  width: 350px;
|]

innerBoxStyle = [style|
  border: 30px solid #eebd53;
  border-radius: 15px;
  width: 430px;
  height: 160px;
  position: relative;
  left: -0px;
|]

outerBoxStyle = [style|
  border: 30px solid #c1db75;
  border-radius: 50px;
  width: 490px;
  margin: 0 auto;
  position: relative;
|]

innerBox item = innerBoxStyle $ div [ item ]
outerBox item = outerBoxStyle $ div [ item ]

green = "#c1db75"
orange = "#eebd53"

alternateColors :: String -> [Purview event m]
alternateColors str =
  let
    letterAndColor = zip str (cycle [green, orange])
  in
    fmap
      (\(letter, color) -> istyle ("color: " <> color) $ span [ text [letter] ] )
      letterAndColor

title str
  = outerBox
  $ innerBox
  $ titleStyle
  $ headlineStyle
  $ h1 str

subtitleStyle = [style|
  font-size: 24px;
  margin: 1rem;
  font-weight: 400;
  color: #514c39;
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
  margin: auto;
  margin-bottom: 75px;
|]

decoratorStyle = [style|
  width: 10px;
  height: 10px;
  border-radius: 5px;
  background-color: black;
  margin-top: 30px;
|]

fancifyStyle = [style|
  display: flex;
|]

fancify :: Purview event m -> Purview event m
fancify item =
  let
    decorator = decoratorStyle $ span []
    decoratorA = istyle ("background-color: " <> green) decorator
    decoratorB = istyle ("background-color: " <> orange) decorator
  in fancifyStyle $ span [ decoratorA, istyle "padding: 0 15px;" item, decoratorB ]

composable = div
  [ fancify $ h3 [ text "Composable" ]
  , p [ text "words words words" ]
  , code [r|
main = do
  putStrLn "hello"
  pure ()
    |]
  ]

familiar = div
  [ fancify $ h3 [ text "Familiar" ]
  , p [ text "The classic event based state management solution blah blah" ]
  ]

effectful = div
  [ fancify $ h3 [ text "Effectful" ]
  , p [ text "yeah it works beautiful with effects blah" ]
  ]

practical = div
  [ fancify $ h3 [ text "Practical" ]
  , p [ text "it's not some re-imagining of the web or some bs" ]
  ]

featuresStyle = [style|
  margin-top: 150px;
  display: flex;
  flex-wrap: wrap;
  justify-content: space-around;
|]

featureStyle = [style|
  text-align: left;
  border-radius: 10px;
  margin: 20px;
  padding: 0px 30px 10px 30px;
  width: 420px;

  div {
    width: 666px;
  }
|]

features = featuresStyle
  $ div
  $ fmap featureStyle
      [ composable
      , familiar
      , effectful
      , practical
      ]

topLevel location = case location of
  "/" -> div
    [ nav
    , topBoxStyle $ div
        [ title $ alternateColors "Purview"
        , dividerStyle $ div []
        , subtitle "Build server rendered, interactive websites with Haskell"
        , features
        ]
    ]
  "/docs" -> div
    [ nav
    , div [ text "docs" ]
    ]
  _ -> div [ text "Page not found" ]


data RouterEvents = SetLocation String

router :: String -> Purview () m
router initialLocation = handler' [] initialLocation reducer topLevel
  where
    reducer (SetLocation "/doc") _ = ("/doc", [])
    reducer (SetLocation "/"   ) _ = ("/",    [])

htmlHeadAdditions = [r|
  <style>
    body {
      margin: 0;
      padding: 0;
      font-family: "Gotham SSm A", "Gotham SSm B", sans-serif;
      font-size: 18px;
      max-width: 65rem;
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
