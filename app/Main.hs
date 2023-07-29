module Main where

import Prelude hiding (div)

import Purview

topLevel = div
  [ text "Purview" ]

router _ = topLevel

main :: IO ()
main = serve defaultConfiguration router
