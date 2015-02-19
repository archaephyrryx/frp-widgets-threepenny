{-# LANGUAGE RecordWildCards #-}

module App.Widgets.Aggregate where

import App.Widgets.Core
import App.Core.Helper
import qualified Graphics.UI.Threepenny as UI
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.List.Split
import App.MicroScope

-- | A data-type that represents an aggregate navigable list of
-- tidelinked values. The two constructors, Case and Cask, represent
-- different approaches to the problem, namely using softlinks and
-- liquidlinks, respectively (pillow-cases store soft things, and casks
-- store liquids). For Cases, the tidelinks are generated once and paged
-- in and out depending on the current view (like a physical book); for
-- Casks, the tidelinks are fixed on the page but mirror the
-- dynamically shifting view with behavioral values (like an e-book).
-- Both of these approaches carry the tidings of the indices that have
-- been selected, which should be reset to -1 (or another negative
-- value) whenever the value should be discarded, and remain so until
-- the next actuation
data Cast = Case { _elementCE :: Element
                 , _actuateCE :: Tidings Int }
          | Cask { _elementCK :: Element
                 , _actuateCK :: Tidings Int}

instance Widget Cast where
    getElement Case{..} = _elementCE
    getElement Cask{..} = _elementCK

userActive :: Cast -> Tidings Int
userActive Case{..} = _actuateCE
userActive Cask{..} = _actuateCK

-- | Create a 'RelNav'.
relNav :: Ord a
    => Behavior ([a]) -- ^ List of items
    -> (Behavior Int, Behavior Int) -- ^ The current ViewMode, as a tuple of (npp, 0-indexed pn)
    -> (a -> String) -- ^ How to name an item
    -> Behavior (a -> LiquidLink Int -> UI Element) -- ^ How to display an item
    -> Behavior ([UI Element] -> UI Element) -- ^ How to aggregate the items
    -> UI (RelNav a)
relNav bGlobal (bScope, bSlide) namer bLinkShow bAggregate = do
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








setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test (Collector)"

    -- Fixed number

    let values = [0..9]
        biteSize = 5
        bits = length values
        bites = bits`cdiv`biteSize

    rec range <- ranger bThis bFirst bLast (pure (string.show.succ)) 
        let tRanger = userLoc range
            eRanger = rumors tRanger
            bRanger = facts tRanger
            bFirst = pure 0
            bLast = (pred) <$> (pure bites)
        bThis <- stepper 0 $ eRanger

    rec range' <- ranger bThis' bFirst' bLast' (pure (string.show.succ)) 
        let tRanger' = userLoc range'
            eRanger' = rumors tRanger'
            bRanger' = facts tRanger'
            bFirst' = pure 0
            bLast' = (pred) <$> (pure bites)
        bThis' <- stepper 0 $ eRanger'

    let chunks = chunksOf biteSize values
        bValues = map (\x -> (!!x) <$> ((chunks!!) <$> bThis)) (enumFromTo 0 (biteSize-1))

    softs <- sequence (zipWith (softLink) (map (numers!!) values) (values))
    liquids <- sequence (zipWith (liquidLink) (replicate biteSize (numers!!)) (bValues))
    
    let chunks' = chunksOf biteSize softs
        bSofts = (chunks'!!) <$> bThis'

    let eSofts = (map (rumors.tideLink) softs)
    let eLiquids = (map (rumors.tideLink) liquids)

    bFoo <- stepper (-1) $ head <$> unions (eSofts++(map ((-1) <$) eLiquids))
    bBar <- stepper (-1) $ head <$> unions (eLiquids++(map ((-1) <$) eSofts))

    val <- UI.h1
    element val # sink UI.text ((cond (>=0) ((:[]).(lettrs!!)) (const "")) <$> bFoo)
    val' <- UI.h1
    element val' # sink UI.text ((cond (>=0) ((:[]).(lettrs!!)) (const "")) <$> bBar)

    softBox <- UI.div
    liquidBox <- UI.div

    element softBox # sink schildren (map (\x -> row [element x]) <$> bSofts)
    element liquidBox # sink schildren (map (\x -> row [element x]) <$> (pure liquids))

    getBody window #+ [ column ([ row [element val], row [element range'] ]++[element softBox])
                      , column ([ row [element val'], row [element range] ]++[element liquidBox])
                      ]



