{-# LANGUAGE QuasiQuotes #-}
-- |

module Pages.Scratch where

import Prelude hiding (div)
-- import Purview (style, ul, li, text, render, Purview, handler, h3, onClick, button, div)
import Purview

listStyle = [style|
  width: 150px;
  li {
    margin-bottom: 25px;
  }
|]

list items = listStyle $ ul $
  fmap (\item -> li [ text item ]) items

rendered = render $ list [ "a list item" ]

data Direction = Up | Down
  deriving (Show, Eq)

countHandler :: (Int -> Purview Direction m) -> Purview () m
countHandler = handler [] 0 reducer
  where
    reducer Up   state = (\newState -> newState + 1, [])
    reducer Down state = (\newState -> newState - 1, [])

view :: Int -> Purview Direction m
view count = div
  [ h3 [ text (show count) ]
  , onClick Up $ button [ text "Increase" ]
  , onClick Down $ button [ text "Decrease" ]
  ]

component :: String -> Purview () m
component _ = countHandler view

y = serve defaultConfiguration component
