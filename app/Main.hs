import System.Environment (getArgs)

import Data.List (sort)
import Data.Map.Lazy (toList)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Transaction
import Utils

wrap :: [a] -> [a] -> [a]
wrap xs ys = xs ++ ys ++ xs

fileOptions :: FileOptions
fileOptions = FileOptions {
  _fo_size = (800, 600),
  _fo_format = SVG
  }


parseArgs :: [String] -> Maybe (FilePath, FilePath)
parseArgs (csvPath:plotPath:_) = Just (csvPath, plotPath)
parseArgs _ = Nothing

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
      showLines $ toList (gather trDest trAmount ts)
      showLines $ toList (gather trMonth trAmount ts)
      showLines $ toList (gather trYear trAmount ts)
      let balance = plot (line "balance" [(accumulate trDate trAmount ts)])
      toFile fileOptions plotPath balance

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  case args of
    Nothing -> putStrLn "Usage: main <csv path> <plot path>"
    Just (csvPath, plotPath) -> main' csvPath plotPath
