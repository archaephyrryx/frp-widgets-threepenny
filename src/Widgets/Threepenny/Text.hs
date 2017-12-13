{-# LANGUAGE RecordWildCards       #-}
module Widgets.Banana.Text where

import Widgets.Banana.Core
import Util hiding (Visible, visible)
import Reactive.ValText
import Control.Monad

data RText = RText { _elemRT :: Element
                   , _textRT :: Behavior String
                   }

instance Widget RText where getElement = _elemRT

rstring :: String -> UI RText
rstring s = do
  _elemRT <- string s
  let _textRT = pure s
  return RText{..}

rtext :: Behavior String -> UI RText
rtext bStr = do
  _elemRT <- UI.span
  element _elemRT # sink text bStr
  let _textRT = bStr
  return RText{..}
