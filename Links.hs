{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module App.Widgets.Links where

import App.Core
import App.Widgets.Core

class LinkLike w where
    tideLink :: w a -> Tidings a

-- * SoftLink * --
-- |A hybrid Link/Button, which can be made into either with CSS rules.
--  The SoftLink stores a single value, which can be hooked to a
--  value-computed action.
--  Useful in contexts where the desired action is deterministic,
--  static, and completely changes the context of the UI, but where
--  the content is rendered instead of cached, making links impractical.
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

liquidLink :: (a -> String) -- Value to display
           -> Behavior a -- Value to hold
           -> UI (LiquidLink a)
liquidLink fdval fluid = do
    link <- button #. "liquidlink"
    element link # sink text (fdval <$> fluid)

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
