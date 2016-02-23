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
    Nothing -> do
      putStrLn ("Could not open file " ++ wrap "'" path ++ ".")
    Just (Left e) -> do
      putStrLn ("Unable to parse " ++ wrap "'" path ++ ".")
      print e
    Just (Right r) -> case selectScheme path of
      Nothing -> putStrLn ("Unable to determine transaction format for " ++ wrap "'" path ++ ".")
      Just scheme -> do
        let transactions = sort $ map (schemeMap scheme) (csvContents r)
        showLines $ toList (gather trDest trAmount transactions)
        showLines $ toList (gather trMonth trAmount transactions)
        showLines $ toList (gather trYear trAmount transactions)
        toFile fileOptions "plot.svg" $
          plot (line "balance" [(accumulate trDate trAmount transactions)])
