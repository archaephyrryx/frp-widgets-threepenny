{-# LANGUAGE RecordWildCards        #-}
module Widgets.Threepenny.Radio where

import Widgets.Threepenny.Links
import Widgets.Threepenny.Core
import qualified Widgets.Threepenny.Core as UI
import Control.Monad (forM_, forM)

data Radio a = Radio { _elementRD :: Element
                     , _tuningsRD :: Tidings a
                     }

instance Widget (Radio a) where
  getElement = _elementRD

radio :: Eq a => [a] -> Behavior a -> (a -> String) -> UI (Radio a)
radio xs bx sf = do
  links <- forM xs (\a -> softLink (sf a) a)

  let eDial = priorityUnion $ map (rumors.tideLink) links
  _elementRD <- UI.div #+ map element links

  forM_ links $ \x -> element x # sink enabled ((/=getCrux x) <$> bx)
  let _tuningsRD = tidings bx eDial
  return Radio{..}
