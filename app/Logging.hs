{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- |

module Logging where

import Data.Time.Clock
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Purview

data Time :: Effect where
  GetCurrentTime :: Time m String

type instance DispatchOf Time = Dynamic

getTime :: Time :> es => Eff es String
getTime = send GetCurrentTime

data Event = GetTime
  deriving (Show, Eq)

reducer
  :: Time :> es
  => Event
  -> String
  -> Eff es (String, [DirectedEvent p Event])
reducer _ _ = do
  time <- getTime
  pure (time, [])

timeHandler :: (String -> View Event) -> View ()
timeHandler = effectHandler' [Self GetTime] "" reducer

runTimeIO
  :: (IOE :> es)
  => Eff (Time : es) a
  -> Eff es a
runTimeIO = undefined

view :: String -> Purview event m
view time = h1 [ text time ]

component :: String -> View ()
component _ = timeHandler view

type View actions = forall es. Time :> es => Purview actions (Eff es)

interpreter' :: Eff '[Time, IOE] a -> IO a
interpreter' = runEff . runTimeIO

main = serve
  defaultConfiguration { interpreter = interpreter' }
  component
