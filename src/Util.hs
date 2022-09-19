module Util(joinPair) where

import Flow((.>))

joinPair :: Applicative f => (f a,b) -> f (a,b)
joinPair = second pure .> uncurry (liftA2 (,))
