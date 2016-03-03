module Utils (loadFile, gather, accumulate,
              injectNothingAs, injectErrorAs,
              deduplicate, wrap) where

import System.IO (openFile, IOMode(ReadMode), hClose)
import System.IO.Strict (hGetContents)
import Control.Exception (bracket, handle, SomeException)
import Data.Map.Lazy (empty, alter, Map)
import Data.List (sortOn)
import qualified Data.Set as Set

-- |Maps nothing to an error representation.
injectNothingAs :: e -> Maybe a -> Either e a
injectNothingAs err Nothing = Left err
injectNothingAs _ (Just x) = Right x

-- |Translates an error representation.
injectErrorAs :: (e1 -> e2) -> Either e1 a -> Either e2 a
injectErrorAs f (Left err) = Left $ f err
injectErrorAs _ (Right x) = Right x

-- |Reads data from specified file immediately
loadFile :: FilePath -> IO (Maybe String)
loadFile path = handle handler $ do
  bracket (openFile path ReadMode) hClose (\h -> Just <$> hGetContents h)
  where handler :: SomeException -> IO (Maybe String)
        handler _ = return $ Nothing

-- |Injects elements into a monoid by key.
gather :: (Ord k, Monoid m) => (a -> k) -> (a -> m) -> [a] -> Map k m
gather key inject xs = foldr ins empty xs
  where ins x = alter (mappend (Just $ inject x)) (key x)

-- |Accumulates elements in order of key
accumulate :: (Ord k, Monoid m) => (a -> k) -> (a -> m) -> [a] -> [(k, m)]
accumulate key inject xs = scanl op (head sorted) (tail sorted)
  where sorted = map (\x -> (key x, inject x)) (sortOn key xs)
        op (_, m) (h, acc) = (h, acc `mappend` m)

-- |Returns a maximal sublist consisting of distinct elements.
deduplicate :: (Ord a) => [a] -> [a]
-- TODO: make this function preserve order.
deduplicate = Set.toList . Set.fromList

-- |Appends and prepends the first argument to the second.
wrap :: [a] -> [a] -> [a]
wrap xs ys = xs ++ ys ++ xs
