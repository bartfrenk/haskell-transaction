{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Transaction.Types (
  Transaction(..), Currency(..), Date(..), Account,
  readTime, trMonth, trYear, trCredit, trDebet) where

import Data.Time (Day, ParseTime, LocalTime(..), TimeOfDay(..), readTime, toGregorian)

import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Axis.LocalTime ()
import Graphics.Rendering.Chart.Axis.Floating ()

import Data.Decimal
import Text.Read (Read(..))
import Data.List (intercalate)
import Numeric (showFFloat)

import Transaction.Plot (transformAxisData)

type Account = String
type Category = String

-- |Currency

newtype Currency = Currency Decimal deriving (Eq, Ord, Num)

instance Monoid Currency where
  mempty = Currency 0
  (Currency x) `mappend` (Currency y) = Currency (x + y)

instance Show Currency where
  -- Intermediate conversion to double might lead to rounding issues.
  show curr = showFFloat (Just 2) (currencyToDouble curr) ""

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

-- |Date

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

-- TODO: maybe collect with trCredit
trDebet :: Transaction -> Currency
trDebet tr = if amount > 0 then amount else 0
  where amount = trAmount tr

trCredit :: Transaction -> Currency
trCredit tr = if amount < 0 then -amount else 0
  where amount = trAmount tr

localTimeToDate :: LocalTime -> Date
localTimeToDate localTime = Date $ localDay localTime

instance PlotValue Date where
  toValue = toValue . dateToLocalTime
  fromValue r = localTimeToDate (fromValue r)
  autoAxis xs = transformAxisData (autoAxis ys) localTimeToDate dateToLocalTime
    where ys = map dateToLocalTime xs

-- |Transaction
type Hash = String

data Transaction = Transaction {
  trDest :: Account,
  trAmount :: Currency,
  trDate :: Date,
  trCat :: Maybe Category,
  trHash :: Hash -- ^ Ensures that equal transactions
                 -- ^ came from identical rows.
  } deriving (Eq)

instance Show Transaction where
  show (Transaction dest amount date cat hash) = intercalate "; " fields
    where fields = ["dest: " ++ show dest,
                    "amount: " ++ show amount,
                    "date: " ++ show date,
                    "category: " ++ show cat,
                    "hash: " ++ show hash]

instance Ord Transaction where
  s `compare` t = (trDate s) `compare` (trDate t)
