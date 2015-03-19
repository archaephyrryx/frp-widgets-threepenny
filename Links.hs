{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module App.Widgets.Links where

import App.Core
import App.Widgets.Core

rclick :: Element -> Event ()
rclick = silence . domEvent "contextmenu"

forbidContext :: Attr Element ()
forbidContext = fromJQueryContextForbid

fromJQueryContextForbid :: Attr Element ()
fromJQueryContextForbid = mkWriteAttr set
    where
    set _ el = runFunction $ ffi "$(%1).contextmenu( function() { return false; });" el

class LinkLike w where
    tideLink :: w a -> Tidings a -- ^ Tidings for normal clicks
    ebbLink :: w a -> Tidings a -- ^ Tidings for right-clicks




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

instance LinkLike SoftLink where
    tideLink sl = let b = (pure (getCrux sl)) in tidings b $ b <@ click (getElement sl)
    ebbLink sl = let b = (pure (getCrux sl)) in tidings b $ b <@ rclick (getElement sl)

softLink :: String -- Value to display
         -> a -- Value to hold
         -> UI (SoftLink a)
softLink dval grist = do
    link <- button #. "softlink" # set text dval # set forbidContext ()

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

instance LinkLike LiquidLink where
    tideLink ll = let b = (getFlux ll) in tidings b $ b <@ click (getElement ll)
    ebbLink ll = let b = (getFlux ll) in tidings b $ b <@ rclick (getElement ll)

getFlux :: LiquidLink a -> Behavior a
getFlux = _fluxLL

-- A type alias for convenience; takes display-function and internal
-- value, returns liquidlink
type LiquidLinker a = Behavior (a -> String) -> Behavior a -> UI (LiquidLink a)

-- | The standard constructor for liquidlinks
liquidLink :: Behavior (a -> String) -- Value to display
           -> Behavior a -- Value to hold
           -> UI (LiquidLink a)
liquidLink bdval fluid = do
    link <- button #. "liquidlink" # set forbidContext ()
    element link # sink text (bdval <*> fluid)

    let _elementLL = link
        _fluxLL = fluid
    return LiquidLink{..}

-- |An advanced, deep abstraction for liquidlinks, which is useful for
-- creating new kinds of liquidLinks (such as Obscurae) but not for user
-- invocation
submerge :: UI Element -- ^ The element to use
         -> String     -- ^ Class that element should have (customization)
         -> WriteAttr Element String -- ^ String-based attribute to modify
         -> LiquidLinker a
submerge el cl att battr fluid = do
    link <- el #. cl
    element link # sink att (battr <*> fluid)

    let _elementLL = link
        _fluxLL = fluid
    return LiquidLink{..}

-- |An infix-able linking function that associates a LiquidLink to its
-- dynamic value-computed action
sinksTo :: LiquidLink a -> (a -> UI ()) -> UI ()
ll`sinksTo`f = on click (getElement ll) $ \_ -> (f =<< (currentValue (getFlux $ ll)))

