{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Transaction (Transaction(..),
                    Currency(..),
                    Date(..),
                    readTime,
                    trMonth,
                    trYear,
                    balance,
                    sumBy,
                    balance') where

import Data.List (intercalate, groupBy)
import Data.Time (Day, ParseTime, LocalTime(..), TimeOfDay(..), readTime, toGregorian)
import Data.Decimal
import Data.Monoid
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Axis.LocalTime ()
import Graphics.Rendering.Chart.Axis.Floating ()
import Text.Read (Read(..))
import Plot (transformAxisData)

type Account = String
type Category = String

newtype Currency = Currency Decimal deriving (Eq, Ord, Show, Num)

instance Monoid Currency where
  mempty = Currency 0
  (Currency x) `mappend` (Currency y) = Currency (x + y)

currencyToDouble :: Currency -> Double
currencyToDouble (Currency decimal) = mantissa / 10 ^ places
  where mantissa = fromIntegral $ decimalMantissa decimal
        places = decimalPlaces decimal

doubleToCurrency :: (RealFrac r) => r -> Currency
doubleToCurrency r = Currency $ realFracToDecimal 2 r

instance PlotValue Currency where
  toValue = currencyToDouble
  fromValue = doubleToCurrency
  autoAxis xs = transformAxisData (autoAxis ys) doubleToCurrency currencyToDouble
    where ys = map currencyToDouble xs

instance Read Currency where
  readPrec = fmap Currency readPrec

newtype Date = Date Day deriving (Eq, Ord, Show, Read, ParseTime)

dateToLocalTime :: Date -> LocalTime
dateToLocalTime (Date day) = LocalTime day $ TimeOfDay 0 0 0

trMonth :: Transaction -> (Integer, Int)
trMonth tr = case trDate tr of
  Date day -> case toGregorian day of
    (y, m, _) -> (y, m)

trYear :: Transaction -> Integer
trYear tr = case trDate tr of
  Date day -> case toGregorian day of
    (y, _, _) -> y

localTimeToDate :: LocalTime -> Date
localTimeToDate localTime = Date $ localDay localTime

instance PlotValue Date where
  toValue = toValue . dateToLocalTime
  fromValue r = localTimeToDate (fromValue r)
  autoAxis xs = transformAxisData (autoAxis ys) localTimeToDate dateToLocalTime
    where ys = map dateToLocalTime xs

data Transaction = Transaction {
  trDest :: Account,
  trAmount :: Currency,
  trDate :: Date,
  trCat :: Maybe Category
  } deriving (Eq)

instance Show Transaction where
  show (Transaction dest amount date cat) = intercalate "; " fields
    where fields = ["dest: " ++ show dest,
                    "amount: " ++ show amount,
                    "date: " ++ show date,
                    "category: " ++ show cat]

instance Ord Transaction where
  s `compare` t = (trDate s) `compare` (trDate t)

balance :: Currency -> [Transaction] -> [(Date, Currency)]
balance _ [] = []
balance initial (t:ts) = (trDate t, current):balance current ts
  where current = initial + trAmount t

balance' :: Currency -> [(Date, Currency)] -> [(Date, Currency)]
balance' _ [] = []
balance' initial ((day, m):ts) = (day, current):balance' current ts
  where current = initial + m


sumBy :: (Eq a) => (Transaction -> a) -> [Transaction] -> [(a, Currency)]
sumBy f ts = map g (groupBy eq ts)
  where eq t s = (f t) == (f s)
        g tss = (f $ head tss, sum (map trAmount tss))
