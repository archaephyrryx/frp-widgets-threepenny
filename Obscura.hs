{-# LANGUAGE DoRec, RecordWildCards #-}

module App.Widgets.Obscura where

import App.Widgets.Core
import App.Widgets.Links
import qualified Graphics.UI.Threepenny as UI
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Vector as V

-- * Obscura * --
-- |Based on the idea of a camera obscura, a widget that acts like a
-- liquidlink but displays an image instead of text; cameras have
-- clickable buttons, and camera obscurae project varying external
-- content onto a photographic plate, in this case a UI element.
-- Otherwise does whatever a liquidlink does
obscura :: Behavior (a -> String) -- Image URL to display
        -> Behavior a -- Value to hold
        -> UI (LiquidLink a)
obscura bCurler fluid = do
    link <- image #. "obscura" # set forbidContext ()
    element link # sink src (bCurler <*> fluid)

    let _elementLL = link
        _fluxLL = fluid
    return LiquidLink{..}

ebbLink :: LiquidLink a -> Tidings a
ebbLink ll = let b = (getFlux ll) in tidings b $ b <@ rclick (getElement ll)

rclick :: Element -> Event ()
rclick = silence . domEvent "contextmenu"

silence = fmap (const ())

forbidContext :: Attr Element ()
forbidContext = fromJQueryContextForbid

fromJQueryContextForbid :: Attr Element ()
fromJQueryContextForbid = mkWriteAttr set
    where
    set _ el = runFunction $ ffi "$(%1).contextmenu( function() { return false; });" el
