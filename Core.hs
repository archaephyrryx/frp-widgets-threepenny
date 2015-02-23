{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module App.Widgets.Core
        ( schildren
        , module App.Core.Helper
        , module Graphics.UI.Threepenny.Core
        , module Graphics.UI.Threepenny.Widgets
        , module Cards.Common.Instances
        , module Control.Applicative
        , module Control.Concurrent.MVar
        , module Control.Monad
        , module Control.Monad.Fix
        , module Control.Monad.IO.Class
        , module Database
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
import Cards.Common.Instances
import Control.Applicative hiding (empty)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Database
import Data.Data (Data, Typeable)
import Data.Dynamic
import Data.Functor
import Data.IxSet
import Data.List hiding (span, drop, delete, union, insert, groupBy, null)
import Data.Map (Map)
import Data.Maybe
import Data.String (fromString)
import Graphics.UI.Threepenny.Core hiding (empty, delete)
import Graphics.UI.Threepenny.Events (click,keydown)
import Graphics.UI.Threepenny.Internal.FFI
import Graphics.UI.Threepenny hiding (size, map, delete, empty)
import Graphics.UI.Threepenny.Widgets
import Reactive.Threepenny hiding (empty)

schildren = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i
