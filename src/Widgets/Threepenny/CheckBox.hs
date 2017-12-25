{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}
module Widgets.Threepenny.CheckBox where

import Widgets.Threepenny.Core
import qualified Widgets.Threepenny.Core as UI
import Control.Monad (forM_, forM)

data CheckBox = CheckBox { _elementCB :: Element
                         , _checkedCB :: Tidings Bool
                         }

instance Widget CheckBox where
  getElement = _elementCB
instance Courier CheckBox Bool where
  tide = _checkedCB

checkbox :: Behavior Bool -> UI CheckBox
checkbox bc  = do
  cb <- UI.input # set (attr "type") "checkbox"

  element cb # sink checked bc

  let _elementCB = cb
      _checkedCB = tidings bc $ checkedChange cb
  return CheckBox{..}
