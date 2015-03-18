{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module App.Widgets.MonoSelect where

import App.Widgets.MultiSelect
import App.Widgets.Core
import App.Core.Helper
import qualified Graphics.UI.Threepenny as UI
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Vector as V

type MonoSelect a = MultiSelect a

selectOnesChange :: Element -> Event [Int]
selectOnesChange el = (maybeToList) <$> (selectionChange' el)

selectOne :: Attr Element [Int]
selectOne = bimapAttr (listToMaybe) (maybeToList) selection

-- | A custom event generator that responds to clicks AND keypresses
selectionChange' :: Element -> Event (Maybe Int)
selectionChange' el = unsafeMapUI el (const $ get selection el) (head <$> unions [ click el, silence . domEvent "keydown" $ el ])

monoSelectD   :: Ord a => Behavior [a] -> Behavior [a] -> Behavior (a -> UI Element) -> Element -- ^ D: the default choice (with presets)
              -> UI (MonoSelect a, Element)

monoSelectD bitems bsel bdisplay noChoice = do
    mono <- UI.select
    element mono # sink (mapschildren (either (return) (\(pdisplay,x) -> UI.option #+ [pdisplay x]))) (((Left noChoice):) <$> (zipWith ((Right .).(,)) <$> (repeat <$> bdisplay) <*> bitems))
    clearbut <- UI.button #. "clear-btn" # settext "clear"

    let bindices = indexify bitems
        indexify = (Map.fromList . flip zip [1..] <$>)
        bsindices = lookupIndex <$> bindices <*> bsel

        lookupIndex indices [] = []
        lookupIndex indices (sel:_) = maybeToList $ Map.lookup sel indices

    element mono # sink selectOne (cond full id (0:) <$> bsindices)

    let bindices2 = Map.fromList . zip [1..] <$> bitems
        _selectionMS = tidings bsel $
            lookupIndex <$> bindices2 <@> (selectOnesChange mono)
        _elementMS   = mono
    return (MultiSelect{..}, clearbut)

monoSelectVDC :: Ord a => Behavior [a] -> Behavior [a] -> Behavior (a -> UI Element) -> UI Element -- ^ VDC: the valid default choice
              -> UI (MultiSelect a, Element)
monoSelectIDC :: Ord a => Behavior [a] -> Behavior [a] -> Behavior (a -> UI Element) -> UI Element -- ^ IDC: the invalid default choice
              -> UI (MultiSelect a, Element)
monoSelectHDC :: Ord a => Behavior [a] -> Behavior [a] -> Behavior (a -> UI Element) -> UI Element -- ^ HDC: the hidden default choice
              -> UI (MultiSelect a, Element)

monoSelectVDC bitems bsel bdisplay pnull = do
    noChoice <- UI.option # set UI.selected True #+ [pnull]
    monoSelectD bitems bsel bdisplay noChoice

monoSelectIDC bitems bsel bdisplay pnull = do
    noChoice <- UI.option # set UI.selected True # set UI.enabled False #+ [pnull]
    monoSelectD bitems bsel bdisplay noChoice

monoSelectHDC bitems bsel bdisplay pnull = do
    noChoice <- UI.option # set UI.selected True # set UI.enabled False # set style [("display","none")] #+ [pnull]
    monoSelectD bitems bsel bdisplay noChoice

-- | Monoselect with Strict Valid Choices
monoSelectSVC :: Ord a
              => Behavior [a]  -- ^ list of items 
              -> Behavior [a]  -- ^ selected items
              -> Behavior (a -> UI Element) -- ^ display for an item
              -> UI (MultiSelect a, Element)
monoSelectSVC bitems bsel bdisplay = do
    mono <- UI.select
    element mono # sink items (map <$> bdisplay <*> bitems)
    clearbut <- UI.button #. "clear-btn" # settext "clear"

    -- animate output selection
    let bindices = indexify bitems
        indexify = (Map.fromList . flip zip [0..] <$>)
        bsindices = lookupIndex <$> bindices <*> bsel

        lookupIndex indices [] = []
        lookupIndex indices (sel:_) = maybeToList $ Map.lookup sel indices

    element mono # sink selectOne bsindices

    -- user selection
    let bindices2 = Map.fromList . zip [0..] <$> bitems
        _selectionMS = tidings bsel $
            lookupIndex <$> bindices2 <@> (selectOnesChange mono)
        _elementMS   = mono
    return (MultiSelect{..}, clearbut)
