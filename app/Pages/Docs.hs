{-# LANGUAGE QuasiQuotes #-}
-- |

module Pages.Docs
  ( component )
where

import Prelude hiding (div)
import Text.RawString.QQ
import Purview
import Code

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
The philosophy is to provide a way, even if it's janky, for you to do whatever
you need to do to get it built.
|]
  , p' [r|
Attributes flow down, events flow up (need a little chart)
|]
  , p' [r|
(list of features)
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
Let's create a submit button from the provided building blocks:
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
Let's style an unordered list.
|]
  , code [rQ|
{-# LANGUAGE QuasiQuotes #-}

import Purview (style, ul, li, text)

listStyle = [style|
  width: 150px;
  li {
    margin-bottom: 25px;
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
Let's break it down with comments.
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
Now let's use it for a counter.  This also shows how you can
create events.
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
Creating events with values in Purview are done with
eh don't love that
|]
  , code [r|
component state = div
  [ [ div [ text state ]
  , onBlur handleInput $ input []
  , onChange handleInput $ input []
  ]

countHandler = handler' [] "" reducer
  where
    reducer (Just text) state = (text, [])
    reducer Nothing     state = ("no text", [])

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
  , code [rQ|
component count = div
  [ receiver "incrementReceiver" (const "increment")
  , class' "counter-display" $ div [ text (show count) ]
  ]

countHandler = handler' [] (0 :: Int) reducer
  where
    reducer "increment" state = (state + 1, [])
    reducer "decrement" state = (state - 1, [])

render = countHandler component

jsCounter = [r|
  const startCount = () => {
    window.setInterval(() => {
      sendEvent("incrementReceiver", "increment")
    }, 1000)
  }
  startCount()

getTest = (defaultConfiguration { eventProducers=[jsCounter] }, render)
|~]
|]
      , code [rQ|
component count = div
  [ class' "counter-display" $ div [ text (show count) ]
  , div [ onClick "increment" $ id' "increment" $ button [ text "increment" ] ]
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

getTest = (defaultConfiguration { eventListeners=[jsMessageAdder] }, render)
|]
  ]

component = div
  [ intro
  , html
  , styling
  , events
  , inputs
  ]
