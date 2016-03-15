module Transaction.Scheme (Scheme, selectScheme, schemeMap, schemeHeaderP) where

import System.FilePath.Posix (takeFileName)
import Data.List (isInfixOf)

import Data.Time (defaultTimeLocale, parseTimeOrError)
import Data.Hash.MD5 (md5s, Str(..))

import Transaction.Types
import Transaction.CSV (Row, HeaderP(..), (!), getCells)

type AList k v = [(k, v)]

findFirst :: (AList k v) -> (k -> Bool) -> Maybe v
findFirst [] _ = Nothing
findFirst alist predicate = case head alist of
  (key, value) -> if predicate key
                  then Just value
                  else findFirst (tail alist) predicate


data Scheme = Scheme {
  schemeMap :: Row -> Transaction,
  schemeHeaderP :: HeaderP
}

replace :: Char -> Char -> String -> String
replace old new text = map substitute text
  where substitute c = if (c == old) then new else c

triodosMap :: Row -> Transaction
triodosMap row = Transaction dest amount date Nothing hash
  where dest = row ! 1
        amount = m $ read (replace ',' '.' (row ! 2))
        date = parseTimeOrError True defaultTimeLocale "%d-%m-%Y" (row ! 0)
        m x = if row ! 3 == "Debet" then -x else x
        hash = md5s $ Str $ concat (getCells row)

rabobankMap :: Row -> Transaction
rabobankMap row = Transaction dest amount date Nothing hash
  where dest = row ! 0
        amount = m $ read (row ! 4)
        date = parseTimeOrError True defaultTimeLocale "%Y%m%d" (row ! 2)
        m x = if (row ! 3) == "D" then -x else x
        hash = md5s $ Str $ concat (getCells row)

schemes :: AList String Scheme
schemes = [("triodos", Scheme triodosMap HasHeader),
           ("rabobank", Scheme rabobankMap HasHeader)]

selectScheme :: FilePath -> Maybe Scheme
selectScheme path = findFirst schemes (filename `contains`)
  where filename = takeFileName path
        s `contains` t = t `isInfixOf` s
