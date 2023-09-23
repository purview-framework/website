{-# LANGUAGE QuasiQuotes #-}
-- |

module Pages.Examples where

import Prelude hiding (div)
import Data.List
import Text.RawString.QQ

import Purview

import Code
import Events
import Logging (Time(GetCurrentTime), runTimePure)

effectfulTime = div
  [ h1 [ text "Effectful Time" ]
  , p [ text "This is a larger example of using effects with purview to get the current server time." ]
  , code [r|
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

import Data.Time.Clock as Clock
import Effectful
import Effectful.Dispatch.Dynamic (send, interpret)
import Purview
import Purview.Server

-- The effect
data Time :: Effect where
  GetCurrentTime :: Time m String

type instance DispatchOf Time = Dynamic

getTime :: Time :> es => Eff es String
getTime = send GetCurrentTime

-- Interpreter for the Time effect
-- This one is impure and get's the real current time
runTimeIO
  :: (IOE :> es)
  => Eff (Time : es) a
  -> Eff es a
runTimeIO = interpret $ \_ -> \case
  GetCurrentTime -> liftIO $ show <$> Clock.getCurrentTime

-- And a pure one, now if you run the reducer or component
-- with this interpreter, it will always be consistent.
runTimePure
  :: (IOE :> es)
  => Eff (Time : es) a
  -> Eff es a
runTimePure = interpret $ \_ -> \case
  GetCurrentTime -> pure "2023-09-23 18:36:15.273405257 UTC"

-- Events we'll use
data Event = GetTime
  deriving (Show, Eq)

-- This is handy for cleaning up how types look,
-- I'd recommend something like it
type View events = forall es. Time :> es => Purview events (Eff es)

reducer
  :: Time :> es
  => Event
  -> String
  -> Eff es (String, [DirectedEvent parentEvent Event])
reducer _ _ = do
  time <- getTime
  pure (time, [])

-- here there's an initial event to load the current time
-- when the handler is loaded
timeHandler :: (String -> View Event) -> View ()
timeHandler = effectHandler' [Self GetTime] "" reducer

view :: String -> Purview event m
view time = h1 [ text time ]

component :: String -> View ()
component _ = timeHandler view

ioInterpreter :: Eff '[Time, IOE] a -> IO a
ioInterpreter = runEff . runTimeIO

pureInterpreter :: Eff '[Time, IOE] a -> IO a
pureInterpreter = runEff . runTimePure

-- and here you could swap interpreters
main = serve
  defaultConfiguration { interpreter = ioInterpreter }
  component
|]]

apiWeather = div
  [ h1 [ text "Getting the Weather" ]
  , p [ text "This is a larger example using weather.gov to fetch the current weather" ]
  , code [r|
import Prelude hiding (div)

import Network.HTTP.Req
import Data.Aeson
import Data.Aeson.Types
import Data.Text

import Purview hiding (render)
import Purview.Server


nameAttr = Attribute . Generic "name"
typeAttr = Attribute . Generic "type"

-----------
-- Model --
-----------

data Day = Day
  { name :: String
  , temperature :: Int
  } deriving (Show, Eq)

newtype Forecast = Forecast [Day]
  deriving (Show, Eq)

instance FromJSON Day where
  parseJSON (Object obj) = Day <$> obj .: "name" <*> obj .: "temperature"
  parseJSON _ = undefined

instance FromJSON Forecast where
  parseJSON (Object obj) = Forecast <$> (obj .: "properties" >>= (.: "periods"))
  parseJSON _ = undefined

----------
-- View --
----------

dayView :: Day -> Purview event m
dayView Day { name, temperature } = div
  [ text $ "the temperature for " <> name <> " is " <> show temperature ]

forecastView :: State -> Purview event m
forecastView state = case state of
  Init                   -> div []
  Loading                -> div [ text "loading" ]
  Loaded (Forecast days) -> div (fmap dayView days)

view :: State -> Purview event m
view state = div
  [ text "Blank"
  , forecastView state
  ]

latLonForm :: Purview (Maybe String) m
latLonForm = onSubmit id $ form
  [ nameAttr "latitude" $ input []
  , nameAttr "longitude" $ input []
  , typeAttr "submit" $ button [ text "Get Weather" ]
  ]

-------------
-- Reducer --
-------------

data Actions
  = LoadWeather String String
  | RequestWeather (Maybe String)
  deriving (Show, Eq)

data State
  = Init
  | Loading
  | Loaded Forecast
  deriving (Show, Eq)

weatherReducer :: Actions -> State -> IO (State, [DirectedEvent parentAction Actions])
weatherReducer (RequestWeather raw) state =
  let
    lat = ""
    lon = ""
  in
    pure (Loading, [Self $ LoadWeather lat lon])
weatherReducer (LoadWeather lat lon) state = do
  forecastLocation <- fetchForecast (lat, lon)

  case forecastLocation of
    Just forecastLocation' -> do
      weather <- fetchWeather forecastLocation'
      pure (Loaded weather, [])
    Nothing ->
      pure (state, [])

weatherHandler :: (State -> Purview Actions IO) -> Purview parentEvent IO
weatherHandler = effectHandler'
  []              -- initial events
  Init            -- initial state
  weatherReducer  -- event reducer

---------
-- API --
---------

userAgent :: Option scheme
userAgent = header "User-Agent" "(purview.org, bontaq@gmail.com)"

fetchWeather :: String -> IO Forecast
fetchWeather forecastLocation = runReq defaultHttpConfig $ do
  res <- req GET
    (https "api.weather.gov" /: "gridpoints" /: "TOP" /: "31,80" /: "forecast")
    NoReqBody
    jsonResponse
    userAgent

  pure (responseBody res :: Forecast)

fetchForecast :: (String, String) -> IO (Maybe String)
fetchForecast (lat, lon) = runReq defaultHttpConfig $ do
  res <- req GET
    (https "api.weather.gov" /: "points" /: (pack lat <> "," <> pack lon))
    NoReqBody
    jsonResponse
    userAgent

  let
    response = responseBody res :: Object
    forecast = parseMaybe
      (\item -> item .: "properties" >>= (.: "forecast")) response :: Maybe String

  pure forecast


------------------
-- All Together --
------------------

render :: Purview parentEvent IO
render = weatherHandler view

main = serve defaultConfiguration (const render)
|]
  ]

pairs =
  [ ("Effectful Time", "effectful-time", effectfulTime)
  , ("API Weather",    "api-weather",    apiWeather)
  ]

buildSidebar location = fmap highlight
  where highlight (title, loc, _) = onClick (SetLocation ("/examples" <> "/" <> loc)) $
          if loc == location
          then istyle "font-weight: 800;" $ li [ text title ]
          else li [ text title ]

pageStyle = [style|
  display: flex;
|]

sidebarStyle = [style|
  min-width: 200px;
  list-style-type: none;
  margin-top: 40px;
  li {
    text-decoration: none;
    padding: 3px;
    &:hover {
      cursor: pointer;
    }
  }
|]

buildPage location =
  let
    sidebarItems = buildSidebar location pairs
    content = maybe
      effectfulTime
      (\(_, _, content) -> content)
      (find (\(title, loc, _) -> loc == location) pairs)
  in pageStyle $ div
     [ sidebarStyle $ ul sidebarItems
     , content
     ]

-- /docs/styles -> styles
parseLocation = drop 1 . dropWhile (/= '/') . drop 1

component location =
  let location' = parseLocation location
  in buildPage location'
