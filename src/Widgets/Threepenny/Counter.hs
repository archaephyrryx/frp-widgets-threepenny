{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE TypeFamilies          #-}

module Widgets.Threepenny.Counter  where

import Util (andM)
import Widgets.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

-- | A counter widget based loosely on the logic of 'Ranger', without any display for the value
-- (pure control). Variants allow for just an incrementor, just a decrementor, a +/- counter, and reset functionality.
--   Incrementor has no decrement
--   Decrementor has no increment
--   Counter has both

data FullControl
  = Incrementor { _inc :: Element }

instance Widget FullControl where
  getElement = _inc


data Counter a = Counter
  { _ctrlCTR :: FullControl
  , _valCTR :: Tidings a
  }
  deriving (Typeable)

instance Widget (Counter a) where
  getElement =  getElement . _ctrlCTR

incre :: UI FullControl
incre = Incrementor <$> UI.button # set text "+"

-- | Vanilla counter: increments by 1, no max value
counter :: (Ord a, Enum a)
        => Behavior a -- ^ Curent value
        -> UI (Counter a)
counter val = do
  fc <- incre
  counter' fc succ val (pure Nothing)


-- | Customizeable counter when using non-standard increments or imposing max value
counter' :: (Ord a, Enum a)
        => FullControl
        -> (a -> a) -- ^ Increment
        -> Behavior a -- ^ Current value
        -> Behavior (Maybe a) -- ^ Maximum value
        -> UI (Counter a)
counter' control step bVal sdMax = do
  let (Incrementor inc) = control

  let bUnder  = (maybe True . (<)) <$> bVal <*> sdMax

  element inc # sink UI.enabled bUnder

  let eInc = click inc

  let eDelta = step <$ whenE bUnder eInc
      eChange = (#) <$> bVal <@> eDelta

      _valCTR  = tidings bVal eChange
      _ctrlCTR = control
  return Counter{..}
