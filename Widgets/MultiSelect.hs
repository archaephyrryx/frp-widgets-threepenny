{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module App.Widgets.MultiSelect where

import App.Widgets.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Vector as V

-- * MultiSelect * --
-- |A customized version of ListBox that allows multiple elements to be
-- selected and provides its own clear-button
data MultiSelect a = MultiSelect
    { _elementMS   :: Element
    , _selectionMS :: Tidings [a]
    }

instance Widget (MultiSelect a) where getElement = _elementMS

-- | User changes to the current selection (possibly empty).
userSelections :: MultiSelect a -> Tidings [a]
userSelections = _selectionMS

-- | Create a 'MultiSelect'.
multiSelect :: Ord a
    => Behavior [a]   -- ^ list of items
    -> Behavior [a]   -- ^ selected items
    -> Behavior (a -> UI Element) -- ^ display for an item
    -> UI (MultiSelect a, Element)
multiSelect bitems bsels bdisplay = do
    multi <- UI.select # set multiple True
    clearbut <- UI.button #. "clear-btn" # settext "clear"

    -- animate output items
    element multi # sink items (map <$> bdisplay <*> bitems)

    -- animate output selection
    let bindices = indexify bitems
        indexify = ((Map.fromList . flip zip [0..]) <$>)
        bsindices   = lookupIndices <$> bindices <*> bsels

        lookupIndices indices [] = []
        lookupIndices indices (sel:selt) = let rest = lookupIndices indices selt
                                           in maybe rest (:rest) (Map.lookup sel indices)

    element multi # sink selections bsindices

    -- changing the display won't change the current selection
    -- eDisplay <- changes display
    -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

    -- user selection
    let bindices2 = Map.fromList . zip [0..] <$> bitems
        _selectionMS = tidings bsels $
            lookupIndices <$> bindices2 <@> (selectionsChange multi)
        _elementMS   = multi
    return (MultiSelect{..}, clearbut)

selectionsChange :: Element -> Event [Int]
selectionsChange el = unsafeMapUI el (const $ get selections el) (click el)

items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ map (\i -> UI.option #+ [i]) i

selections :: Attr Element [Int]
selections = fromJQuerySelectedIndices from (JSON.toJSON)
    where
    from s = let (JSON.Success x) = JSON.fromJSON s in x

fromJQuerySelectedIndices :: (JSON.Value -> [Int]) -> ([Int] -> JSON.Value) -> Attr Element [Int]
fromJQuerySelectedIndices from to = mkReadWriteAttr get set
    where
    set v el = runFunction $ ffi "$(%1).val( (%2).map(function(x) { return $.makeArray($(%1).find('option').map(function() { return $(this).val(); }))[x]))" el (to v)
    get   el = fmap from $ callFunction $ ffi "$(%1).val().map(function(x) { return $.inArray(x, $.makeArray($(%1).find('option').map(function() { return $(this).val(); }))); }) || []" el

clearSels :: Attr Element ()
clearSels = fromJQueryClearSel

fromJQueryClearSel :: Attr Element ()
fromJQueryClearSel = mkWriteAttr set
    where
      set _ el = runFunction $ ffi "$(%1).find('option:selected').removeAttr('selected')" el
