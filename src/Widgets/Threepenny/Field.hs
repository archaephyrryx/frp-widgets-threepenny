{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Widgets.Threepenny.Field where

import Text.Read (readMaybe)
import Data.Wrapped
import Widgets.Threepenny.Core
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Vector as V
import Util

-- * Field * --
-- |A polymorphic input field
data Field a = Field
  { _elementFL :: Element
  , _currentFL :: Tidings (Maybe a)
  }

legibleField :: (Read a, Show a)
             => Behavior (Maybe a)
             -> UI (Field a)
legibleField bVal = do
    fld <- input # set (attr "type") "text"
    element fld # sink value ((show?/) <$> bVal)

    let _currentFL = tidings bVal $ readMaybe <$> valueChange fld
        _elementFL = fld
    return Field{..}

hintField :: Hint a
          => Behavior (Maybe a) -- ^ Optional minimum value
          -> Behavior (Maybe a) -- ^ Optional maximum value
          -> Behavior (Maybe a) -- ^ Current value
          -> UI (Field a)
hintField bmin bmax bval = do
    hfld <- input # set (attr "type") "number" # set (attr "step") "1"

    element hfld # sink (attr "min") ((showH?/) <$> bmin)
    element hfld # sink (attr "max") ((showH?/) <$> bmax)
    element hfld # sink value ((showH?/) <$> bval)

    let _currentFL = tidings bval $ readMaybeH <$> (clickValueChange hfld `also` valueChange hfld)
        _elementFL = hfld
    return Field{..}
