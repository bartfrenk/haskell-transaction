module Scheme (selectScheme) where

import System.FilePath.Posix (takeFileName)
import Data.List (isInfixOf)
import System.Locale (defaultTimeLocale)

import CSV
import Transaction
import Utils (AList, findFirst)

type Scheme = Row -> Transaction

replace :: Char -> Char -> String -> String
replace old new text = map substitute text
  where substitute c = if (c == old) then new else c

triodos :: Scheme
triodos row = Transaction dest amount date Nothing
  where dest = row ! 1
        amount = m $ read (replace ',' '.' (row ! 2))
        date = readTime defaultTimeLocale "%d-%m-%Y" (row ! 0)
        m x = if row ! 3 == "Debet" then -x else x

rabobank :: Scheme
rabobank row = Transaction dest amount date Nothing
  where dest = row ! 0
        amount = m $ read (row ! 4)
        date = readTime defaultTimeLocale "%Y%m%d" (row ! 2)
        m x = if (row ! 3) == "D" then -x else x

schemes :: AList String Scheme
schemes = [("triodos", triodos),
           ("rabobank", rabobank)]


selectScheme :: FilePath -> Maybe Scheme
selectScheme path = findFirst schemes (filename `contains`)
  where filename = takeFileName path
        s `contains` t = t `isInfixOf` s
