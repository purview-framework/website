{-# LANGUAGE QuasiQuotes #-}
-- |

module Pages.Scratch where

import Prelude hiding (div)
-- import Purview (style, ul, li, text, render, Purview, handler, h3, onClick, button, div)
import Data.Typeable
import Purview
import Purview.Server

-- onKeyDown
--   :: ( Typeable event
--      , Eq event
--      , Show event
--      )
--   => (Maybe String -> event) -> Purview event m -> Purview event m
-- onKeyDown = Attribute . On "keydown" Nothing

-- newConfig =
--   let events = eventsToListenTo defaultConfiguration
--   in defaultConfiguration { eventsToListenTo="keydown":events }
--
-- main = serve newConfig $ const (div [])

-- view direction = onClick "toggle" $ button [ text direction ]
--
--
-- reducer :: String -> String -> (String -> String, [DirectedEvent parentEvent String])
-- reducer event state = case event of
--   "up"   -> (const "down", [])
--   "down" -> (const "up", [])
--
-- toggleHandler :: (String -> Purview String m) -> Purview parentEvent m
-- toggleHandler = handler [] "up" reducer
--
-- component :: Purview parentEvent m
-- component = toggleHandler view

-- x = serve defaultConfiguration component


data CountEvent = Increment | Decrement
  deriving (Show, Eq)

view :: Int -> Purview CountEvent m
view count = div
  [ h1 [ text (show count) ]
  , div [ onClick Increment $ button [ text "increment" ]
        , onClick Decrement $ button [ text "decrement" ]
        ]
  ]

-- arguments are initial actions, initial state, and then the reducer
countHandler = handler' [] (0 :: Int) reducer
  where
    reducer Increment state = (state + 1, [])
    reducer Decrement state = (state - 1, [])

-- url is passed in to the top level component by `serve`
component url = countHandler view

main = serve defaultConfiguration component
