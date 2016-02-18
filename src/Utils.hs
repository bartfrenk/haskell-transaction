module Utils where

import Data.Map.Lazy (empty, alter, Map)
import Data.Monoid
import Data.List (sortBy)

type AList k v = [(k, v)]

findFirst :: (AList k v) -> (k -> Bool) -> Maybe v
findFirst [] _ = Nothing
findFirst alist predicate = case head alist of
  (key, value) -> if predicate key
                  then Just value
                  else findFirst (tail alist) predicate

showLines :: (Show a) => [a] -> IO ()
showLines = mapM_ (putStrLn . show)

-- |Injects elements into a monoid by key.
gather :: (Ord k, Monoid m) => (a -> k) -> (a -> m) -> [a] -> Map k m
gather key inject xs = foldr ins empty xs
  where ins x = alter (mappend (Just $ inject x)) (key x)

-- Available in Data.List from 4.8.0
sortOn :: (Ord k) => (a -> k) -> [a] -> [a]
sortOn key xs = sortBy cmp xs
  where x `cmp` y = (key x) `compare` (key y)

-- |Accumulates elements in order of key
accumulate :: (Ord k, Monoid m) => (a -> k) -> (a -> m) -> [a] -> [(k, m)]
accumulate key inject xs = scanl op (head sorted) (tail sorted)
  where sorted = map (\x -> (key x, inject x)) (sortOn key xs)
        op (k, m) (h, acc) = (h, acc `mappend` m)
