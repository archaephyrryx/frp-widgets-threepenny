{-# LANGUAGE RecordWildCards #-}

module App.Widgets.RelNav where

import App.Widgets.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.List.Split
import App.MicroScope

-- | A 'Relative Navigator', which allows a user to navigate through a
-- long list of items, but displaying only a certain number of items at
-- a time, in a customizable way; this is tied indirectly to a ViewMode
-- and Ranger

data RelNav a = RelNav
  { _elementRN :: Element
  , _currentRN :: Tidings (MicroScope a)
  }

instance Widget (RelNav a) where getElement = _elementRN

userFocus :: RelNav a -> Tidings (MicroScope a)
userFocus = _currentRN

-- | Create a 'RelNav'.
relNav :: (Ord a, Widget b)
    => Behavior ([a]) -- ^ List of items
    -> Behavior (Int, Int) -- ^ The dynamic ViewMode (npp, pn)
    -> Ranger Int -- ^ The dynamic ViewMode
    -> Behavior (a -> b -> UI Element) -- ^ How to display an item
    -> Behavior ([UI Element] -> UI Element) -- ^ How to aggregate the items
    -> UI (RelNav a)
relNav bGlobal bVMode bShower bAggregate = do
    let
        modeFlat = 0
        modeList = 1
        modeGrid = 2

        modeType :: ViewMode -> Int
        modeType FlatView{..} = modeFlat
        modeType ListView{..} = modeList
        modeType GridView{..} = modeGrid
        
        bModeType = modeType <$> bVMode
        bNpp = npp <$> bVMode
        bPageN = pn <$> bVMode

    microBox <- UI.div

    let bChunks = chunksOf <$> bScope <*> bGlobal
        bMicro = Tunneling <$> bChunks <*> bSlide
        bFocus = focus <$> bMicro
        bFirst = head <$> bFocus

    bLinks <- sequence (zipWith (liquidLink) (namer.(!!) <$> bFocus <*>) [0..])

    let eBlinks = map (rumors.tideLinks) bLinks

    
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
