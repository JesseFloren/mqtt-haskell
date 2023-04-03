module Utils.Map where

import qualified Data.Map as M

-- | Reverse of Map.lookup. Looks up keys by their value
lookupKey :: Eq v => M.Map k v -> v -> [k]
lookupKey m val = M.foldrWithKey go [] m where
  go key value found =
    if value == val
    then key:found
    else found

toB :: (Ord k, Show k) => M.Map k v -> k -> v
toB mp k | Just v <- k `M.lookup` mp = v
toB _ k = error ("unknown value" ++ show k)

fromB :: (Ord v, Show v) => M.Map k v -> v -> k
fromB mp v | [k] <- lookupKey mp v = k
fromB _ v = error ("unknown value" ++ show v)
