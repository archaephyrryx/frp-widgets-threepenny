{-# LANGUAGE RecordWildCards, DoRec #-}

module App.Widgets.Ranger where

import Control.Applicative (Applicative)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Functor
import Data.IORef
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.IxSet
import Data.List
import Data.Data ( Data, Typeable)
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Internal.FFI
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Elements   as UI
import qualified Graphics.UI.Threepenny.Events     as UI
import Graphics.UI.Threepenny.Events               (click,keydown)
import qualified Graphics.UI.Threepenny.Core       as UI
import Reactive.Threepenny
import Reactive.Threepenny hiding (onChange)
import App.Core.Helper

-- | A 'Ranger', which consists of an incrementor and decrementor for a
-- dynamic range and dynamic value within that range; works for any
-- bounded enumerable value
data Ranger a = Ranger
  { _elementRG :: Element
  , _currentRG :: Tidings a
  }

instance Widget (Ranger a) where getElement = _elementRG

userLoc :: Ranger a -> Tidings a
userLoc = _currentRG

-- | Create a 'RelNav'.
ranger :: (Ord a, Enum a)
    => Behavior a -- ^ Current location
    -> Behavior a -- ^ 'Zero' value
    -> Behavior a -- ^ Maximum value (zero-indexed)
    -> Behavior (a -> UI Element) -- ^ display for an item
    -> UI (Ranger a)
ranger bloc bzer bmax bdisplay = do
    prev <- UI.button #. "ranger-but prev" # settext "<"
    cur <- UI.div #. "ranger-item cur"
    next <- UI.button #. "ranger-but next" # settext ">"
    box <- UI.div #+ [element prev, element cur, element next]

    let bNotFirst = (>) <$> bloc <*> bzer 
        bNotLast  = (<) <$> bloc <*> bmax 

    element prev # sink UI.enabled bNotFirst
    element cur # sink curview ((.(:[])).map <$> bdisplay <*> bloc)
    element next # sink UI.enabled bNotLast

    let bLocApp = (#) <$> bloc
        ePrev = click prev
        eNext = click next
        eChange = head <$> unions (map (apply bLocApp) $
            [ pred <$ whenE bNotFirst ePrev
            , succ <$ whenE bNotLast  eNext
            ])
        
        _currentRG = tidings bloc eChange
        _elementRG   = box
    return Ranger{..}

curview = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ map (\i -> UI.span #+ [i]) i
