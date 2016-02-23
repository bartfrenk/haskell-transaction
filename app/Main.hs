import System.Environment (getArgs)


import Data.List (sort)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Transaction
import Data.Map.Lazy (toList)
import Utils


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
  result <- loadTransactions path
  case result of
    Left FileNotFound ->
      putStrLn ("Could not open file " ++ wrap "'" path ++ ".")
    Left (InvalidCSV err) ->
      putStrLn ("Unable to parse " ++ wrap "'" path ++ ".") >> print err
    Left UnknownScheme ->
      putStrLn ("Unable to determine transaction format for " ++ wrap "'" path ++ ".")
    Right ts' -> do
      let ts = sort ts'
      showLines $ toList (gather trDest trAmount ts)
      showLines $ toList (gather trMonth trAmount ts)
      showLines $ toList (gather trYear trAmount ts)
      let balance = plot (line "balance" [(accumulate trDate trAmount ts)])
      toFile fileOptions "plot.svg" balance
