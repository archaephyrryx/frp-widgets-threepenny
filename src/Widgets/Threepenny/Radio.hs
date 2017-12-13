{-# LANGUAGE RecordWildCards        #-}
module Widgets.Banana.Radio where

import Widgets.Banana.Links
import Widgets.Banana.Table
import Widgets.Banana.Core
import Control.Monad (forM_, forM)

data Radio a = Radio { _elementRD :: Element
                     , _tuningsRD :: Tidings a
                     }

instance Widget (Radio a) where
  getElement = _elementRD

radio :: Eq a => [a] -> Behavior a -> (a -> String) -> UI (Radio a)
radio xs bx sf = do
  links <- forM xs (\a -> softLink (sf a) a)

  let eDial = unionWith const (map (rumors.tideLink) links)

  forM_ links (\x -> element x # sink UI.enabled $ (/=getCrux x) <$> bx)
  let _elementRD = row 5 $ map widget links
      _tuningsRD = tidings bx eDial
  return Radio{..}
