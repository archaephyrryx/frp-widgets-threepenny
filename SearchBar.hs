{-# LANGUAGE RecordWildCards #-}

module App.Widgets.SearchBar where

import App.Widgets.Core
import qualified Graphics.UI.Threepenny as UI
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Vector as V

-- * SearchBar * --
-- |A search-bar that performs auto-completed lookups into an IxSet
data SearchBar a = SearchBar
    { _elementSB :: Element
    , _searchsSB :: Tidings String
    , _matchesSB :: Tidings ([a])
    }

instance Widget (SearchBar a) where getElement = _elementSB

userSearch :: SearchBar a -> Tidings (String)
userSearch = _searchsSB

searchMatches :: SearchBar a -> Tidings ([a])
searchMatches = _matchesSB

searchBar :: (Ord a, Indexable a, Typeable a)
    => Behavior (IxSet a)         -- ^ list of items
    -> Behavior String            -- ^ partial search
    -> Behavior (a -> UI Element) -- ^ display for an item
    -> UI (SearchBar a)
searchBar reftab pstr rdisplay = do
    sbar <- UI.input
    sres <- UI.ul
    scomb <- UI.form #. "search-bar" #+ [element sbar, element sres]
    let
        doSearch rt ps = toList (rt @+ wds)
            where
              wds :: [Nameword]
              wds = map ravel . words $ ps

    element sbar # set UI.type_ "search"
    element sbar # set (attr "placeholder") "Search"
    element sbar # sink UI.value pstr

    element sres #. "dropdown-menu"
    element sres # sink qmatches (map <$> rdisplay <*> (doSearch <$> reftab <*> pstr))

    -- user selection
    let
        _matchesSB = tidings (doSearch <$> reftab <*> pstr) $
            doSearch <$> reftab <@> UI.valueChange sbar
        _searchsSB = tidings pstr $ UI.valueChange sbar
        _elementSB   = scomb

    return SearchBar{..}

qmatches = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ map (\i -> UI.li #+ [i]) i
