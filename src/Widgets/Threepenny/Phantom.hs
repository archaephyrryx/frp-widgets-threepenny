{-# LANGUAGE RecordWildCards           #-}

-- | A module for displaying one of several different widgets in a static frame, modally
module Widgets.Threepenny.Phantom where

import qualified Graphics.UI.Threepenny as UI
import Widgets.Threepenny.Core
import Control.Monad (forM_, sequence_, forM, void)
import Util

-- | A widget class for one possible contents of a static frame. `Edifice` defines contents that are
-- visible whenever they are in focus, while `Artifice` defines contents that may be invisible even
-- while in focus. Each aspect should contain a unique equatable polymorphic ADT tag so that it may be
-- invoked by the `Phantom` widget.
data Aspect a =  Edifice { _construct :: Element
                         , _epithet   :: a
                         }
              | Artifice { _construct :: Element
                         , _epithet   :: a
                         , _transient :: Behavior Bool
                         }

-- | Unified constructor for the `Aspect` data-type, with a Maybe-enclosed boolean behavior, which
-- serves as the transience parameter for Artifices when present
aspect :: (Widget w, Enum a) => w -> a -> Maybe (Behavior Bool) -> Aspect a
aspect c e (Just bv) = Artifice (getElement c) e bv
aspect c e Nothing   = Edifice (getElement c) e

invisibleTruth :: Bool -> [(String,String)]
invisibleTruth = consd id [] [("display","none")]

-- | `eclipse` defines the conditional visibility of an `Aspect` in the UI monad.
eclipse :: (a -> Behavior Bool) -> Aspect a -> UI ()
eclipse f (Edifice c e) = void $ element c # sink UI.style (invisibleTruth <$> f e)
eclipse f (Artifice c e t) = void $ element c # sink UI.style (invisibleTruth <$> ((&&) <$> f e <*> t))


instance Widget (Aspect a) where
  getElement = _construct

-- | Widget consisting of all of the possible aspects of a static frame, which focuses only one at a
-- time (though the focused aspect may be invisible)
data Phantom a = Phantom { _elementPH :: Element
                         , _aspects :: [Aspect a]
                         , _manifest :: Behavior a
                         }

instance Widget (Phantom a) where
  getElement = _elementPH

-- | Create a `Phantom` widget from a list of aspects and an invocation behavior
phantom :: (Enum a, Eq a) => [Aspect a] -> Behavior a -> UI (Phantom a)
phantom _aspects _manifest = do
  let nascence = (<$> _manifest) . (==)
  forM_ _aspects (eclipse nascence)
  _elementPH <- UI.div
  widget _elementPH # set children (map _construct _aspects)
  return Phantom{..}



-- | Phantom widgets only control the visibility of their Aspects, and do not control their layout.
-- The `Reaper` widget encapsulates the Panel in which all widgets of all Aspects of a Phantom are
-- created, and uses the layout of the nascent aspect as its own layout. This means that no
-- invisible widgets not in the current Aspect affect the layout of other widgets.
data Reaper a = Reaper { _field   :: Element
                       , _harvest :: Phantom a
                       , _specter :: Behavior Element
                       }

instance Widget (Reaper a) where
  getElement = _field

-- | Create a reaper from a table and a phantom
reap :: (Enum a, Eq a) => Phantom a -> UI (Reaper a)
reap Phantom{..} = do
  let apparition = (\x -> _construct . only . filter (_epithet.=x) $ _aspects) <$> _manifest

  plot <- UI.div #. "reaper"

  element plot # sink children (one <$> apparition)

  return $ Reaper plot Phantom{..} apparition

{-
-- Refresh the display of a specter
reharvest :: Reaper a -> Event () -> UI ()
reharvest Reaper{..} = relay _field {- do
  let applicator = (\x -> \() -> refresh x >> windowReLayout (_tab$_field)) <$> _specter
      eFresh = applicator <@> e
  reactimate eFresh

-}
-}
