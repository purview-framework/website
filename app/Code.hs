-- | Component for displaying code

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

code str = Html "pre" [Html "code" [ text str ]]
