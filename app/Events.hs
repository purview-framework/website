-- |

module Events
  ( RouterEvents(..) )
where

data RouterEvents = SetLocation String
  deriving (Eq, Show)
