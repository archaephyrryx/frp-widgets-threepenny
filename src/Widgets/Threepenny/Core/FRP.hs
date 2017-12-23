module Widgets.Threepenny.Core.FRP where

import Reactive.Threepenny

also :: Event a -> Event a -> Event a
also = unionWith const

-- | priorityUnion
-- Combine a list of events, giving earlier elements of the list higher precedence on simultaneous triggers
priorityUnion :: [Event a] -> Event a
priorityUnion = foldl1 also
