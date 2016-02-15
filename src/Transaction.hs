module Transaction (balance, Transaction(..), sumBy, balance') where
-- only export Transaction

import Data.List (intercalate, groupBy)
import Data.Time (Day)
import Data.Decimal

type Account = String
type Category = String
type Currency = Decimal

formatCurrency :: Currency -> String
formatCurrency = show

data Transaction = Transaction {
  destination :: Account,
  amount :: Currency,
  date :: Day,
  category :: Maybe Category
  } deriving (Eq)

instance Show Transaction where
  show t = intercalate "; " fields
    where fields = ["dest: " ++ show (destination t),
                    "amount: " ++ formatCurrency (amount t),
                    "date: " ++ show (date t),
                    "category: " ++ show (category t)]

instance Ord Transaction where
  s `compare` t = (date s) `compare` (date t)

balance :: Currency -> [Transaction] -> [(Day, Currency)]
balance _ [] = []
balance initial (t:ts) = (date t, current):balance current ts
  where current = initial + amount t

balance' :: Currency -> [(Day, Currency)] -> [(Day, Currency)]
balance' _ [] = []
balance' initial ((day, m):ts) = (day, current):balance' current ts
  where current = initial + m


sumBy :: (Eq a) => (Transaction -> a) -> [Transaction] -> [(a, Currency)]
sumBy f ts = map g (groupBy eq ts)
  where eq t s = (f t) == (f s)
        g tss = (f $ head tss, sum (map amount tss))
