{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Widgets.Banana.Recorder where

import Widgets.Banana.Core
import Util hiding (Visible, visible)
import Control.Monad
import Widgets.Banana.Input
import Widgets.Banana.Text
import Widgets.Banana.Table
import Widgets.Banana.Fields
import Widgets.Banana.Links

data Recorder = Recorder { _elementRE :: Element
                         , _fileioRE :: Tidings String
                         }
                         deriving (Typeable)


instance Widget Recorder where
  getElement = _elementRE

lwidget Recorder{..} = row 5 [ widget _save , widget _load ]

recorder :: Behavior String -- ^ String value to be saved/loaded
         -> Behavior String -- ^ String value to display for save button
         -> Behavior String -- ^ String value to display for load button
         -> Behavior String -- ^ Filepath to save to / load from
         -> MomentIO Recorder
recorder w bVal bLabs bLabl bPath = do
  -- the internal store of the liquidlink is opaquely used for the path and content
  -- as those are the arguments to writeFile
  save <- liquidLink (const <$> bLabs) ((,) <$> bPath <*> bVal)

  -- similarly, only just the path for readFiel
  load <- liquidLink (const <$> bLabl) bPath

  save`sinksTo`uncurry writeFile
  let eLoad = load`triggerEvent`readFile
  let _store = tidings bVal eLoad
  return Recorder{..}
