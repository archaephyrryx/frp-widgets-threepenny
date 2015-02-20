{-# LANGUAGE RecordWildCards, DoRec #-}

module App.Widgets.Cast where

import App.Widgets.Core
import App.Widgets.Links
import App.Widgets.Ranger
import App.Core.Helper
import qualified Graphics.UI.Threepenny as UI
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.List.Split

-- | The 'Cast' typeclass is an abstraction for any widget that presents
-- a list of items that can be actuated, but do not have any persistent
-- active status.
--
-- A 'Cast' widget presents a list of active UI elements and responds to
-- an element being clicked by generating an event containing the 0-indexed
-- position of that item overall. Unlike a list-box, elements are actuated
-- rather than toggled, and no reference is made to the underlying type of
-- the data being stored; translating the indices into elements is trivially
-- left to the caller. A value of -1 is set initially, and can be used
-- to \'reset\' the selection once it has been processed, if persistence
-- is not desired.
--
-- There are two instances of this typeclass, namely 'Case' and
-- 'Cask', which are internally built upon the 'SoftLink' and
-- 'LiquidLink' widgets, which are currently the two instances of the
-- LinkLike class. The behavioral distinctions of SoftLink and
-- LiquidLink are partially carried over to the 'Case' and Cask
-- aggregates, in that Casks can be used more dynamically than Cases,
-- just as LiquidLinks can be used more dynamically than SoftLinks.
-- The primary manifestation of this difference is that Casks can be
-- created from dynamic lists (or pure monadic constant lists), whereas
-- Cases can only be built from pure static lists. However, Cases can
-- handle an FR value for the number of items per page, whereas Casks
-- can only be built from a pure constant pagesize.
-- 
-- Casts are called such because liquids or soft semi-solids can be set
-- into a cast and hardened.
class Cast m where
    userActive :: m -> Tidings Int

-- | The 'Case' data-type represents an aggregate navigable list of
-- softlinked values, and is one of the instances of the class 'Cast'.
--
-- The implementation of 'Case' is best modeled by a book, which has a
-- certain fixed list of pages that can be displayed one at a time (\'page\'
-- here is used loosely); when a book is printed, all of the pages are
-- created, but only the current one is shown at any given time. The
-- Case approach is to transform a pure constant list into a pure
-- constant list of SoftLinks storing the index of their corresponding
-- list element, and changing which of these prefabricated softlinks are
-- shown at any given time. 
--
-- Cases are called such because certain varieties of case (e.g.
-- pillow-cases) hold soft objects, and all cases store solid objects.
data Case = Case { _elementCE :: Element
                 , _actuateCE :: Tidings Int }

-- | The 'Cask' data-type represents an aggregate navigable list of
-- liquidlinked values, and is one of the instances of the class 'Cast'.
--
-- The implementation of 'Cask' is best modeled by an e-reader, which
-- has a single screen that is fixed in place but presents different
-- pages of an e-book without any physical page-flips; when an e-reader
-- is assembled, none of the pages of the books it will eventually store
-- are created, but each page is rendered on the single screen as
-- required. The Cask approach is to focus a dynamic list into a
-- dynamic-sublist of the desired size, and
-- create that same number of LiquidLinks which take on the values of
-- their respective elements in the dynamic sublist through a
-- transformation @Behavior [a] -> [Behavior a]@. Because this list is
-- immutable, it is necessary for it to have a fixed size.
-- 
-- Casks are called such because casks store liquids.
data Cask = Cask { _elementCK :: Element
                 , _actuateCK :: Tidings Int}

instance Widget Case where
    getElement = _elementCE
instance Cast Case where
    userActive = _actuateCE

instance Widget Cask where
    getElement = _elementCK
instance Cast Cask where
    userActive = _actuateCK

blTranspose :: Int -> a -> Behavior [a] -> [Behavior a]
blTranspose n z bxs = map (\x -> (!!x) <$> (take n . (++(repeat z)) <$> bxs)) (enumFromTo 0 (n-1))

lbTranspose :: [Behavior a] -> Behavior [a]
lbTranspose = foldr (\x acc -> (:) <$> x <*> acc) (pure [])


-- | Cast builder for Cases only, which relies on a static list to
-- display sections of; the page size is an FR (possibly pure) value
softCast :: [a] -- ^ Full list
         -> Behavior Int -- ^ Number per page
         -> (a -> String) -- ^ Label for the softlinks
         -> (a -> (SoftLink Int -> UI Element)) -- ^ Row transformer for items
         -> UI Case
softCast lFull bBiteSize label fRower = do
    let bits = length lFull
        values = [0..(bits-1)]
        bBites = (bits`cdiv`) <$> bBiteSize

    rec range <- ranger bThis bFirst bLast (pure (string.show.succ))
        let tRanger = userLoc range
            eRanger = rumors tRanger
            bRanger = facts tRanger
            bFirst = pure 0
            bLast = pred <$> bBites
        bThis <- stepper 0 $ eRanger

    softs <- sequence (zipWith (softLink) (map (label) lFull) (values))
    
    let bChunks = chunksOf <$> bBiteSize <*> (pure softs)
        eSofts = (map (rumors.tideLink) softs)
        bSofts = (!!) <$> bChunks <*> bThis
        eActua = head <$> unions (eSofts++[ (-1) <$ eRanger ])

    softBox <- UI.table
    element softBox # sink schildren (zipWith ($) (map fRower lFull) <$> bSofts)

    wrapper <- column [ row [element softBox ], row [element range] ]

    let _elementCE = wrapper
        _actuateCE = tidings (pure (-1)) $ eActua
    return Case{..}

-- | Cast builder for Casks only, which relies on an FR (possibly pure) list to display
-- sections of; the page size must be a pure integer value
liquidCast :: Behavior [a] -- ^ Full list
           -> Int -- ^ Number per page (cannot be FR)
           -> (a -> String) -- ^ Label for the liquidlinks
           -> (a -> (LiquidLink Int -> UI Element)) -- ^ Row transformer for items
           -> UI Cask
liquidCast bFull biteSize label fRower = do
    let values = (map fst . zip [0..] <$> bFull)
        bBits = length <$> values
        bBites = ((`cdiv`biteSize) <$> bBits)

    rec range <- ranger bThis bFirst bLast (pure (string.show.succ))
        let tRanger = userLoc range
            eRanger = rumors tRanger
            bRanger = facts tRanger
            bFirst = pure 0
            bLast = pred <$> bBites
        bThis <- stepper 0 $ eRanger

    let
        bChunks = chunksOf biteSize <$> values
        bValues = blTranspose biteSize (-1) ((!!) <$> bChunks <*> bThis)

    liquids <- sequence (zipWith liquidLink (replicate biteSize ((.) <$> (pure label) <*> (((.abs).(!!)) <$> bFull))) bValues)
    
    let eLiquids = (map (rumors.tideLink) liquids)
        eActua = head <$> unions (eLiquids++[ (-1) <$ eRanger])

    liquidBox <- UI.table
    element liquidBox # sink schildren (zipWith ($) <$> (map fRower <$> bFull) <*> (filtrate <$> (lbTranspose $ map (((>=0) <$>).getFlux) liquids) <*> (pure liquids)))
    wrapper <- column [ row [ element liquidBox ], row [element range] ]

    let _elementCK = wrapper
        _actuateCK = tidings (pure (-1)) $ eActua
    return Cask{..}
