{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module App.Widgets.Obscura where

import App.Core
import App.Widgets.Core
import App.Widgets.Links

-- * Obscura * --
-- |Based on the idea of a camera obscura, a widget that acts like a
-- liquidlink but displays an image instead of text; cameras have
-- clickable buttons, and camera obscurae project varying external
-- content onto a photographic plate, in this case a UI element.
-- Otherwise does whatever a liquidlink does
data SoftLink a = SoftLink
  { _elementSL :: Element
  , _cruxSL :: a
  }

instance Widget (SoftLink a) where getElement = _elementSL

softLink :: String -- Value to display
         -> a -- Value to hold
         -> UI (SoftLink a)
softLink dval grist = do
    link <- button #. "softlink" # set text dval

    let _elementSL = link
        _cruxSL = grist
    return SoftLink{..}

getCrux :: SoftLink a -> a
getCrux = _cruxSL

-- |An infix-able linking function that associates a SoftLink to its
-- value-computed action
linksTo :: SoftLink a -> (a -> UI ()) -> UI ()
sl`linksTo`f = on click (getElement sl) $ \_ -> f (getCrux sl)

-- | Mutable-content softlink
data LiquidLink a = LiquidLink
  { _elementLL :: Element
  , _fluxLL :: Behavior a
  }

instance Widget (LiquidLink a) where getElement = _elementLL

liquidLink :: Behavior (a -> String) -- Value to display
           -> Behavior a -- Value to hold
           -> UI (LiquidLink a)
liquidLink bdval fluid = do
    link <- button #. "liquidlink"
    element link # sink text (bdval <*> fluid)

    let _elementLL = link
        _fluxLL = fluid
    return LiquidLink{..}

getFlux :: LiquidLink a -> Behavior a
getFlux = _fluxLL

-- |An infix-able linking function that associates a LiquidLink to its
-- dynamic value-computed action
sinksTo :: LiquidLink a -> (a -> UI ()) -> UI ()
ll`sinksTo`f = on click (getElement ll) $ \_ -> (f =<< (currentValue (getFlux $ ll)))

-- |Link tidings: constant on click, changes with crux/flux

instance LinkLike SoftLink where
    tideLink sl = let b = (pure (getCrux sl)) in tidings b $ b <@ click (getElement sl)

instance LinkLike LiquidLink where
    tideLink ll = let b = (getFlux ll) in tidings b $ b <@ click (getElement ll)
