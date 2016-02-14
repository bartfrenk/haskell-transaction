module Scheme (selectScheme) where

import qualified Transaction as T
import CSV (Row, (!))
import System.FilePath.Posix (takeFileName)
import Data.List (isInfixOf)
import Data.Time (Day, readTime)
import System.Locale (defaultTimeLocale)

type AList k v = [(k, v)]
type Scheme = Row -> T.Transaction

replace :: Char -> Char -> String -> String
replace old new text = map substitute text
  where substitute c = if (c == old) then new else c

triodos :: Scheme
triodos row = T.Transaction destination (m * amount) time Nothing
  where destination = row ! 1
        amount = read (replace ',' '.' (row ! 2)) :: Double
        m = if (row ! 3 == "Debet") then (-1) else 1
        time = readTime defaultTimeLocale "%d-%m-%Y" (row ! 0) :: Day

rabobank :: Scheme
rabobank row = T.Transaction destination (m * amount) time Nothing
  where destination = row ! 0
        amount = read (row ! 4) :: Double
        m = if (row ! 3 == "D") then (-1) else 1
        time = readTime defaultTimeLocale "%Y%m%d" (row ! 2) :: Day

schemes :: AList String Scheme
schemes = [("triodos", triodos),
           ("rabobank", rabobank)]

findFirst :: (AList k v) -> (k -> Bool) -> Maybe v
findFirst [] _ = Nothing
findFirst alist predicate = case head alist of
  (key, value) -> if predicate key
                  then Just value
                  else findFirst (tail alist) predicate

selectScheme :: FilePath -> Maybe Scheme
selectScheme path = findFirst schemes (filename `contains`)
  where filename = takeFileName path
        s `contains` t = t `isInfixOf` s
