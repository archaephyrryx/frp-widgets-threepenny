{-# LANGUAGE RecordWildCards #-}

module App.Widgets.RelNav where

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
import qualified Graphics.UI.Threepenny.Core       as UI
import Reactive.Threepenny
import Reactive.Threepenny hiding (onChange)
import Cards.Common.Hint
import Cards.Common.Stringe
import Cards.Common.Abbrev
import Database
import App.Core.Helper
import App.MicroScope

-- | A 'Relative Navigator', which allows a user to navigate through a
-- long list of items, but displaying only the current and its
-- neighbors, as well as buttons to move around.
data RelNav a = RelNav
  { _elementRN :: Element
  , _currentRN :: Tidings a
  }

instance Widget (RelNav a) where getElement = _elementRN

userFocus :: RelNav a -> Tidings (MicroScope a)
userFocus = _currentRN

-- | Create a 'RelNav'.
relNav ::
    => Set a   -- ^ List of items
    -> Behavior a   -- ^ selected item
    -> Behavior (a -> UI Element) -- ^ display for an item
    -> UI (RelNav a)
relNav glob bcur bdisplay = do
    preb <- UI.div #. "relnav-item before"
    curb <- UI.div #. "relnav-item cur"
    folb <- UI.div #. "relnav-item after"
    nbox <- UI.div #+ [element preb, element curb, element folb]

    fbut <- UI.button #. "relnav-but first" # settext "<<"
    pbut <- UI.button #. "relnav-but prev" # settext "<"
    nbut <- UI.button #. "relnav-but next" # settext ">"
    lbut <- UI.button #. "relnav-but last" # settext ">>"
    wrapper <- UI.div #+ [ UI.span #+ [element fbut, element pbut]
                         , element nbox
                         , UI.span #+ [element nbut, element lbut]
                         ]

    -- animate output selection
    let bMicro = rescopeS <$> narrow bcur
        narrow x = (\(x,z) y -> (reverse.Set.toAscList$x, y, Set.toAscList$z)) <$> (Set.split <$> x <*> (pure glob)) <*> x
        btrip = (\s@Scanning{..} -> [head hs, x, head ts]) <$> bMicro

    element nbox # sink rview (map <$> bdisplay <*> btrip)

    -- changing the display won't change the current selection
    -- eDisplay <- changes display
    -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

    -- user selection
    let _currentRN = tidings bcur $
            focus <$> ((#) <$> bMicro <@> (onionWith const (curStart fbut) (curBack pbut) (curForth nbut) (curEnd lbut)))
        _elementMS   = wrapper

    return RelNav{..}

onionWith :: (a -> a -> a) -> Event a -> Event a -> Event a -> Event a -> Event a
onionWith f i j k l = let onion = unionWith f in (i`onion`j)`onion`(k`onion`l)

curStart :: Element -> Event Adjustment
curStart el = unsafeMapUI el (const $ goFirst) (UI.click el)
curBack :: Element -> Event Adjustment
curBack el = unsafeMapUI el (const $ goBack) (UI.click el)
curForth :: Element -> Event Adjustment
curForth el = unsafeMapUI el (const $ goNext) (UI.click el)
curEnd :: Element -> Event Adjustment
curEnd el = unsafeMapUI el (const $ goLast) (UI.click el)

rview = mkWriteAttr $ \i x -> void $ do
    k <- return x # get children
    return x # set children zipWith (\x y -> x # set children [] #+ [y]) k i
