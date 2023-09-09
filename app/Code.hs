-- | Component for displaying code
{-# LANGUAGE QuasiQuotes #-}

module Code
  ( code )
where

import Purview

{-

data Counter = Increment | Decrement

reducer action state = case action of
  Increment -> (state + 1, [])
  Decrement -> (state - 1, [])

countHandler = mkHandler { initailActions = [], initialState = 0, reducer = reducer }

view state = div
  [ h1 [ text (show state) ]
  , button [ text "increment" ]
  , button [ text "decrement" ]
  ]

component = countHandler view

-}

codeStyle = [style|
  border-radius: 10px;
  background-color: #fcf5bf;
  padding: 0px 20px 20px 20px;
|]

code str = codeStyle $ Html "pre" [Html "code" [ text str ] ]
