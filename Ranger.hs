{-# LANGUAGE RecordWildCards, RecursiveDo #-}

module App.Widgets.Ranger where

import App.Widgets.Core
import qualified Graphics.UI.Threepenny as UI

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
    cur <- UI.span #. "ranger-item cur"
    next <- UI.button #. "ranger-but next" # settext ">"
    box <- UI.span #+ [element prev, element cur, element next]

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
