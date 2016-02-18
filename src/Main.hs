import System.Environment (getArgs)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import System.IO
import Control.Exception (bracket, handle, SomeException)
import qualified System.IO.Strict as Strict

import Text.ParserCombinators.Parsec
import Data.List (sort)

import CSV (csv, Doc, csvContents, HeaderP(..))
import Transaction
import Scheme (selectScheme)
import Utils (gather, accumulate, showLines)
import Data.Map.Lazy (toList)


parseTransactions :: HeaderP -> FilePath -> IO (Maybe (Either ParseError Doc))
parseTransactions hasHeader path = handle handler $ do
  bracket (openFile path ReadMode) hClose $ \h -> do
    contents <- Strict.hGetContents h
    return (Just (parse (csv hasHeader) path contents))
  where handler :: SomeException -> IO (Maybe (Either ParseError Doc))
        handler _ = return Nothing

wrap :: [a] -> [a] -> [a]
wrap xs ys = xs ++ ys ++ xs

fileOptions :: FileOptions
fileOptions = FileOptions {
  _fo_size = (800, 600),
  _fo_format = SVG
  }

main :: IO ()
main = do
  (path:_) <- getArgs
  result <- parseTransactions HasHeader path
  case result of
    Nothing -> do
      putStrLn ("Could not open file " ++ wrap "'" path ++ ".")
    Just (Left e) -> do
      putStrLn ("Unable to parse " ++ wrap "'" path ++ ".")
      print e
    Just (Right r) -> case selectScheme path of
      Nothing -> putStrLn ("Unable to determine transaction format for " ++ wrap "'" path ++ ".")
      Just scheme -> do
        let transactions = sort $ map scheme (csvContents r)
        showLines $ toList (gather trDest trAmount transactions)
        showLines $ toList (gather trMonth trAmount transactions)
        showLines $ toList (gather trYear trAmount transactions)
        toFile fileOptions "plot.svg" $
          plot (line "balance" [(accumulate trDate trAmount transactions)])
