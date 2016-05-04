import System.Environment (getArgs)

import Data.List (sort)
import Data.Map.Lazy (toList)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Transaction
import Utils

fileOptions :: FileOptions
fileOptions = FileOptions {
  _fo_size = (800, 600),
  _fo_format = SVG
  }

main' :: FilePath -> FilePath -> IO ()
main' csvPath plotPath = do
  result <- loadTransactionFile csvPath
  case result of
    Left FileNotFound ->
      putStrLn ("Could not open file " ++ wrap "'" csvPath ++ ".")
    Left (InvalidCSV err) ->
      putStrLn ("Unable to parse " ++ wrap "'" csvPath ++ ".") >> print err
    Left UnknownScheme ->
      putStrLn ("Unable to determine transaction format for " ++ wrap "'" csvPath ++ ".")
    Right ts' -> do
      let ts = sort ts'
      ts'' <- getTransactions "test.db"
      showLines $ toList (gather trDest trAmount ts)
      showLines $ toList (gather trMonth trAmount ts)
      showLines $ toList (gather trYear trAmount ts)
      putStrLn $ show (length ts'')
      putStrLn $ show (length ts)
      let balance = plot (line "balance" [(accumulate trDate trAmount ts)])
      toFile fileOptions plotPath balance
  where

showLines :: (Show a) => [a] -> IO ()
showLines = mapM_ (putStrLn . show)

parseArgs :: [String] -> Maybe (FilePath, FilePath)
parseArgs (csvPath:plotPath:_) = Just (csvPath, plotPath)
parseArgs _ = Nothing

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  case args of
    Nothing -> putStrLn "Usage: main <csv path> <plot path>"
    Just (csvPath, plotPath) -> main' csvPath plotPath
