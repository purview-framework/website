{-# LANGUAGE QuasiQuotes #-}
-- |

module Pages.Docs
  ( component )
where

import Prelude hiding (div)
import Text.RawString.QQ
import Purview
import Code
import Events
import Data.Foldable (find)
import Data.Maybe (fromMaybe)

import Debug.Trace
import Purview.Server (defaultConfiguration)

-- topcs =
--   [ intro
--   , html
--   , style
--   , events
--   , inputs (use inputs)
--   , forms (use forms)
--   , interop
--   ]

p' str = p [ text str ]

intro = div
  [ h1 [ text "Introduction" ]
  , p' [r|
Purview is a server side rendered framework for building websites with Haskell.
|]
  , p' [r|
It takes the best parts from several different frameworks and uses Haskell's power
to improve upon them.  Server side rendering with websockets from Elixir's Phoenix
LiveView, reducers from Redux, easy styling from Styled Components, message handling
from Elm, and effects from Redux Sagas.
|]
  , p' [r|
The improvements we get from Haskell and the design of this framework are easier
composition of components, complete type safety of events, no need for one monolithic
state, clarity over event origination and handling, and easy testability (using algebraic
effects instead of sagas).
|]
  , p' [r|
As a little example of what it looks like:
|]
  , code [r|
module Main where

import Prelude hiding (div)
import Purview
import Purview.Server


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
    reducer Increment state = (state + 1, [])  -- new events can be added in the []
    reducer Decrement state = (state - 1, [])

-- url is passed in to the top level component by `serve`
component url = countHandler view

main = serve defaultConfiguration component
|]
  , p' [r|
This is a pretty new framework so feel free to make suggestions (and expect bugs)!
|]
         ]

html = div
  [ h1 [ text "HTML" ]
  , p' [r|
HTML in Purview works a little differently than in other libraries or
frameworks. Instead of adding attributes to an array, attributes are
composed above the concrete HTML and added to it.
|]
  , code [r|
import Purview (a, href, render)

link = href "https://purview-hs.org" $ a [ text "a link!" ]

rendered = render link
-- rendered is <a href="https://purview-hs.org">a link!</a>
|]
  , p' [r|
Since HTML and supported attributes change, Purview does not come with a complete
array of node types and attributes.  You'll find yourself creating your
own frequently as you bend Purview to your style.
|]
  , p' [r|
Here's how you can create a submit button from the provided building blocks:
|]
  , code [r|
import Purview (Html, Attribute, Generic, render)

-- first we need a button HTML node
button :: [Purview event m] -> Purview event m
button = Html "button"

renderedButton = render $ button [ text "hello" ]
-- renderedButton is <button>hello</button>

-- now we need an attribute to set the type
typeAttr :: String -> Purview event m -> Purview event m
typeAttr = Attribute . Generic "type"

-- that one is pretty generic, so for type="submit", we can do
submitAttr :: Purview event m -> Purview event m
submitAttr = typeAttr "submit"

-- and put it together
submitButton = submitAttr $ button [ text "submit" ]

rendered = render submitButton
-- rendered is <button type="submit">submit</button>
|]
  , p' [r|
Above any Html you can stack as many attributes as you need.  It's a bit
experimental in style but I've been finding it fun and with some benefits
-- namely, not having to explicitly pass in attributes or attribute configuration
to lower level components.
|]
  ]

styling = div
  [ h1 [ text "Styling" ]
  , p' [r|
Borrowing something nice from styled components in React land, you can
style components with template Haskell.  Invisibly the styles are turned
into classes that are added to the component, and in my opinion makes
for a fun way to write CSS.
|]
  , p' [r|
Here's how you can style an unordered list, showing support for basic
nesting and psuedo-selectors.
|]
  , code [rQ|
{-# LANGUAGE QuasiQuotes #-}

import Purview (style, ul, li, text)

listStyle = [style|
  width: 150px;
  li {
    margin-bottom: 25px;
    &:hover {
      cursor: pointer;
    }
  }
|~]

list items = listStyle $ ul $
  fmap (\item -> li [ text item ]) items

-- and then used like
myList = list [ "hello" ]

-- renders as a <ul class="p123...(a hash of the css)"><li>hello</li></ul>
|]
  , p' [r|
This feature is extremely alpha and doesn't support inlining
variables, so for styles where you need variability use the built
in istyle helper.  It provides real inline styles.
|]
  , code [r|
changingColorButton ready =
  let style' = if ready then "color: green;" else ""
  in istyle style' $ button [ text "go!" ]

-- renders as <button style="color: green;">go!</button>
--         or <button style="">go!</button>
|]
  ]

events = div
  [ h1 [ text "Events & State" ]
  , p' [r|
Events in Purview bubble up to Handlers, which can then change state.  The
state from Handlers is passed back down to components.  Let's look at the
type for a pure handler.
|]
  , code [r|
import Purview (handler)

x :: (Typeable event, Typeable state, Show state, Eq state)
  => [DirectedEvent parentEvent event]
  -> state
  -> (event -> state -> (state -> state, [DirectedEvent parentEvent event]))
  -> (state -> Purview event m)
  -> Purview parentEvent m
x = handler
|]
  , p' [r|
Breaking it down with comments:
|]
  , code [r|
import Purview (handler)

x :: (Typeable event, Typeable state, Show state, Eq state)

  1. The initial events.  This can be used to send a message to
     itself or the parent on loading, for example to kick off
     an API request.

  => [DirectedEvent parentEvent event]

  2. The initial state

  -> state

  3. The reducer.  This is what will handle any events received.

  -> (event -> state -> (state -> state, [DirectedEvent parentEvent event]))

  4. The continuation, or the component which takes in the state.

  -> (state -> Purview event m)

  5. The result type.  Notice that it's typed by what events the Handler
     can produce, keeping type safety no matter where you plug this in.

  -> Purview parentEvent m

x = handler
|]
  , p' [r|
Here's how you can use it for a counter.  This also shows how you can
create events with onClick.
|]
  , code [r|
import Purview (handler, div, h3, text, button, onClick)

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

component :: Purview () m
component = countHandler view
|]
  , p' [r|
Events bubble up to handlers, just like in the browsers, and are
captured by handlers.
|]
         ]

inputs = div
  [ h1 [ text "Inputs" ]
  , p' [r|
Working with inputs in Purview is similar to working with onClick.  These
take an additional argument to transform the input, or value gotten from
the input, into an event for the handler.  In this that's handleInput.
|]
  , code [r|
handleInput (Just val) = val
handleInput Nothign    = ""

component state = div
  [ [ div [ text state ]
  , onBlur handleInput $ input []
  , onChange handleInput $ input []
  ]

countHandler = handler' [] "" reducer
  where
    reducer text state = (text, [])

render = countHandler component
|]
  ]

forms = div
  [ h1 [ text "Forms" ]
  , code [r|
submitButton = typeAttr "submit" $ button [ text "submit" ]

component state = div
  [ div [ id' "text-display" $ div [ text state ] ]
  , onSubmit id $ form
    [ id' "text-field" textField
    , id' "text-submit" submitButton
    ]
  ]

countHandler = handler' [] "" reducer
  where
    reducer (Just text) state = (text, [])
    reducer Nothing     state = ("no text", [])

render = countHandler component
|]
  ]

interop = div
  [ h1 [ text "Interop" ]
  , p' [r|
In this example, each second an event is received from Javascript.  It's
matched by name to the receiver, which turns it into an event for the
handler.
|]
  , code [rQ|
component count = div
  [ class' "counter-display" $ div [ text (show count) ]
  ]

countHandler = handler' [] (0 :: Int) reducer
  where
    reducer "increment" state = (state + 1, [])
    reducer "decrement" state = (state - 1, [])

countReceiver = receiver "incrementReceiver" (const "increment")

render = countHandler . countReceiver $ component

jsCounter = [r|
  const startCount = () => {
    window.setInterval(() => {
      sendEvent("incrementReceiver", "increment")
    }, 1000)
  }
  startCount()
|~]

main = serve defaultConfiguration { javascript=jsCounter } render
|]
  , p' [r|
And in this example, each time you click the "increment" button, an event
is sent to the browser where addMessage is called.  It sets the value of
the div with id "messages" to the latest count.
|]
  , code [rQ|
component count = div
  [ div [ text (show count) ]
  , div [ onClick "increment" $ button [ text "increment" ] ]
  , id' "messages" $ div []
  ]

countHandler = handler' [] (0 :: Int) reducer
  where
    reducer "increment" state =
      let newState = state + 1
      in (newState, [Browser "addMessage" (show newState)])

render = countHandler component

jsMessageAdder = [r|
  const addMessage = (value) => {
    const messagesBlock = document.querySelector("#messages");
    messagesBlock.innerHTML = value;
  }
  window.addMessage = addMessage;
|~]

main = server defaultConfiguration { javascript=jsMessageAdder } render
|]
  ]

starterStyle = [style|
  font-size: inherit;
  a {
    text-decoration: underline;
  }
|]

starter = starterStyle $ div
  [ h1 [ text "Getting Started" ]
  , p' [r|
  This part assumes you don't currently have Haskell setup.  First, you'll want to
  install <a target="_blank" href="https://www.haskell.org/ghcup/">GHCUp</a>.  This is a nice way to manage Haskell's various tools and versions.
|]
  , p' [r|
  Next, you'll want to choose an editor that works well with Haskell.  The easiest to
  setup is VSCode, where all you need to do is add the Haskell plugin.  I use
  <a target="_blank" href="https://github.com/doomemacs/doomemacs">doom eMacs</a> which takes
  a little more work but is more responsive.
|]
  , p' [r|
  Now you'll want to clone the <a target="_blank" href="https://github.com/purview-framework/purview-template">starter template</a>.  This contains a basic
  example to get started with.  Before running it, you'll want to use GHCup to select your version
  of GHC.  Choose the recommended version, which as of this writing is 9.4.7, although it should work
  with newer versions as well.  To select and install it, just do:
|]
  , code [r|
  ghcup tui
|]
  , p' [r|
  In your terminal.  Once you've got everything set up, you can build and run the project with:
|]
  , code [r|
  cabal build
  cabal exec purview-template
|]
  ]

pairs =
  [ ("Intro",       "intro",   intro)
  , ("Get Started", "start",   starter)
  , ("HTML",        "html",    html)
  , ("Styling",     "styling", styling)
  , ("Events",      "events",  events)
  , ("Inputs",      "inputs",  inputs)
  , ("Forms",       "forms",   forms)
  , ("Interop",     "interop", interop)
  ]

buildSidebar location = fmap highlight
  where highlight (title, loc, _) = onClick (SetLocation ("/docs" <> "/" <> loc)) $
          if loc == location
          then istyle "font-weight: 800;" $ li [ text title ]
          else li [ text title ]

pageStyle = [style|
  display: flex;
|]

sidebarStyle = [style|
  min-width: 150px;
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
      intro
      (\(_, _, content) -> content)
      (find (\(title, loc, _) -> loc == location) pairs)
  in pageStyle $ div
     [ sidebarStyle $ ul sidebarItems
     , content
     ]

-- /docs/styles -> styles
parseLocation = drop 1 . dropWhile (/= '/') . drop 1

component location =
  let location' = parseLocation (trace location $ location)
  in buildPage location'
