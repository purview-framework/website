{-# LANGUAGE QuasiQuotes #-}
-- |

module Pages.Scratch where

import Purview (style, ul, li, text, render)

listStyle = [style|
  width: 150px;
  li {
    margin-bottom: 25px;
  }
|]

list items = listStyle $ ul $
  fmap (\item -> li [ text item ]) items

rendered = render $ list [ "a list item" ]
