module Widgets.Threepenny.Core.FRP where

import Reactive.Threepenny

-- | priorityUnion
-- Combine a list of events, giving earlier elements of the list higher precedence on simultaneous triggers
priorityUnion :: [Event a] -> Event a
priorityUnion = foldl1 (unionWith const)
