{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Widgets.Threepenny.Recorder where

import Widgets.Threepenny.Core
import qualified Widgets.Threepenny.Core as UI
import Util hiding (Visible, visible)
import Control.Monad
import Widgets.Threepenny.Text
import Widgets.Threepenny.Links

data Recorder = Recorder { _elementRE :: Element
                         , _fileioRE :: Tidings String
                         }
                         deriving (Typeable)


instance Widget Recorder where
  getElement = _elementRE


recorder :: Behavior String -- ^ String value to be saved/loaded
         -> Behavior String -- ^ String value to display for save button
         -> Behavior String -- ^ String value to display for load button
         -> Behavior String -- ^ Filepath to save to / load from
         -> UI Recorder
recorder bVal bLabs bLabl bPath = do
  -- the internal store of the liquidlink is opaquely used for the path and content
  -- as those are the arguments to writeFile
  save <- liquidLink (const <$> bLabs) ((,) <$> bPath <*> bVal)

  -- similarly, only just the path for readFiel
  load <- liquidLink (const <$> bLabl) bPath

  save`sinksTo`uncurry (\x y -> liftIO $ writeFile x y)
  let eLoad = load`triggerEvent`readFile

  _elementRE <- UI.span
  element _elementRE #+ [element save, element load]

  let _fileioRE = tidings bVal eLoad
  return Recorder{..}
