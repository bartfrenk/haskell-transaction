import System.Environment (getArgs)

--import Transaction as T
import System.IO
import Control.Exception (bracket, handle, SomeException)
import qualified System.IO.Strict as Strict

import CSV (csv, Doc, csvContents, HeaderP(..))
import Scheme (selectScheme)
import Text.ParserCombinators.Parsec

parseTransactions :: HeaderP -> FilePath -> IO (Maybe (Either ParseError Doc))
parseTransactions hasHeader path = handle handler $ do
  bracket (openFile path ReadMode) hClose $ \h -> do
    contents <- Strict.hGetContents h
    return (Just (parse (csv hasHeader) path contents))
  where handler :: SomeException -> IO (Maybe (Either ParseError Doc))
        handler _ = return Nothing

wrap :: [a] -> [a] -> [a]
wrap xs ys = xs ++ ys ++ xs

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
      Just scheme -> mapM_ print (map scheme (csvContents r))
