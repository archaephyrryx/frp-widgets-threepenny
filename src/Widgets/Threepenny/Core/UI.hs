module Widgets.Threepenny.Core.UI where

import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.Attributes
import Reactive.Threepenny
import Util

glue :: UI Element
glue = string (" " :: String)

hlink :: String -> String -> UI Element
hlink url str = a # set href url # settext str

pss :: Show a => Behavior (a -> UI Element)
pss = pure (string . show)

psss :: (Show a, Enum a) => Behavior (a -> UI Element)
psss = pure (string . show . succ)

plss :: Show a => Behavior (a -> UI Element)
plss = pure (estring li . show)

noop :: UI Element
noop = a

estring :: UI Element -> String -> UI Element
estring el = (el #+) . once string

bstring = estring bold
istring = estring italics

settext = set text
