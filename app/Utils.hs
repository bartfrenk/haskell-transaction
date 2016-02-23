module Utils
       ( loadTransactions, gather, accumulate )
       where

import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import Control.Exception (bracket, handle, SomeException)
import Data.Map.Lazy (empty, alter, Map)
import Data.List (sortOn)

import Control.Applicative (liftA2)
import Control.Monad (join)

import Transaction (Transaction, ParseError, parseTransactions, selectScheme)


showLines :: (Show a) => [a] -> IO ()
showLines = mapM_ (putStrLn . show)

data LoadError = FileNotFound | InvalidCSV ParseError | UnknownScheme
               deriving (Show)

injectNothingAs :: LoadError -> Maybe a -> Either LoadError a
injectNothingAs e Nothing = Left e
injectNothingAs _ (Just x) = Right x

injectParseResult :: Either ParseError a -> Either LoadError a
injectParseResult (Left e) = Left $ InvalidCSV e
injectParseResult (Right x) = Right x

loadTransactions :: FilePath -> IO (Either LoadError [Transaction])
loadTransactions path = fmap (join . (parseTransactions' <$> scheme' <*>)) input'
  where scheme' = injectNothingAs UnknownScheme $ selectScheme path
        input' = injectNothingAs FileNotFound <$> loadFile path
        parseTransactions' scheme input = injectParseResult (parseTransactions scheme input)

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
