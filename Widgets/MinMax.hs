{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module App.Widgets.MinMax where

import App.Widgets.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Vector as V

clickValueChange :: Element -> Event String
clickValueChange el = unsafeMapUI el (const $ get value el) (mouseKey el)

-- * Min and Max * --
-- |A "minimum value" numeric input field
data Min a = Min
    { _elementMN   :: Element
    , _nuMN :: Tidings (Maybe a)
    }

-- |A "maximum value" numeric input field
data Max a = Max
    { _elementMX   :: Element
    , _nuMX :: Tidings (Maybe a)
    }

instance Widget (Min a) where getElement = _elementMN
instance Widget (Max a) where getElement = _elementMX

-- | User changes to the current values (possibly empty).
userMin :: Min a -> Tidings (Maybe a)
userMin = _nuMN
userMax :: Max a -> Tidings (Maybe a)
userMax = _nuMX

minmax :: Hint a
    => Behavior (Maybe a) -- ^ Minimum value
    -> Behavior (Maybe a) -- ^ Maximum value
    -> Behavior (a -> String) -- ^ display for an item
    -> UI (Min a, Max a)
minmax bmin bmax bdisplay = do
    mini <-  UI.input # set (attr "type") "number" # set (attr "step") "1" # set (attr "placeholder") "Min" # set (attr "min") "0"
    maxi <-  UI.input # set (attr "type") "number" # set (attr "step") "1" # set (attr "placeholder") "Max" # set (attr "min") "0"

    -- animate output items
    element mini  # sink value ((maybe ("")) <$> bdisplay <*> bmin)
    element maxi  # sink (attr "min") ((maybe ("0")) <$> bdisplay <*> bmin)
    element maxi  # sink value ((maybe ("")) <$> bdisplay <*> bmax)

    let _nuMN = tidings bmin $ readMaybeH <$> clickValueChange mini
        _elementMN   = mini
        _nuMX = tidings bmax $ readMaybeH <$> clickValueChange maxi
        _elementMX   = maxi
    return (Min{..}, Max{..})
