{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-} --, NoMonomorphismRestriction #-}
module App.Widgets.Core
        ( kinder
        , mapkinder
        , silence
        , mouseKey
        , unsafeMapUI
        , module App.Core.Helper
        , module Graphics.UI.Threepenny.Core
        , module Graphics.UI.Threepenny.Widgets
        , module CCG.Cards.Common.Instances
        , module Control.Applicative
        , module Control.Concurrent.MVar
        , module Control.Monad
        , module Control.Monad.Fix
        , module Control.Monad.IO.Class
        , module API.Database
        , module Data.Data
        , module Data.Dynamic
        , module Data.Functor
        , module Data.IxSet
        , module Data.List
        , module Data.Map
        , module Data.Maybe
        , module Data.String
        , module Graphics.UI.Threepenny
        , module Reactive.Threepenny
        ) where

import App.Core.Helper
import CCG.Cards.Common.Instances
import Control.Applicative hiding (empty)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import API.Database
import Data.Data (Data, Typeable)
import Data.Dynamic
import Data.Functor
import Data.IxSet hiding (Proxy)
import Data.List hiding (span, drop, delete, union, insert, groupBy, null)
import Data.Map (Map)
import Data.Maybe
import Data.String (fromString)
import Graphics.UI.Threepenny.Core hiding (empty, delete)
import Graphics.UI.Threepenny.Events
import Graphics.UI.Threepenny hiding (size, map, delete, empty)
import Graphics.UI.Threepenny.Widgets
import Reactive.Threepenny hiding (empty)

silence :: Functor f => f a -> f ()
silence = fmap (const ())

unsafeMapUI el f = unsafeMapIO (\a -> getWindow el >>= \w -> runUI w (f a))

-- | Attribute generator with highly general type polymorphism
kinder :: (a -> [UI Element]) -> WriteAttr Element a
kinder f = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ (f i)

-- | Attribute generator for display functions that are mapping in
-- nature
mapkinder :: (a -> UI Element) -> WriteAttr Element [a]
mapkinder f = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ (map f i)

-- | An event representing either a 'keydown' event or a 'click' event,
-- for widgets that can change values through either action; in the case
-- of keydown, the value is recorded
mouseKey :: Element -> Event (Maybe KeyCode) -- ^ (Maybe a) is isomorphic to (Either () a), and it is simpler
mouseKey el = head <$> unions [ Nothing <$ click el, Just <$> keydown el ]
