{-# LANGUAGE RecursiveDo, RecordWildCards #-}

module Widgets.Threepenny.Obscura where

import Widgets.Threepenny.Core
import Widgets.Threepenny.Links
import qualified Graphics.UI.Threepenny as UI
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Vector as V

obscura :: Behavior (a -> String) -- Image URL to display
        -> Behavior a -- Value to hold
        -> UI (LiquidLink a)
obscura bCurler fluid = do
    link <- image #. "obscura" # set forbidContext ()
    element link # sink src (bCurler <*> fluid)

    let _elementLL = link
        _fluxLL = fluid
    return LiquidLink{..}
