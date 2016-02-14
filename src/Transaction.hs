module Transaction where
-- only export Transaction

import Data.List (intercalate)
import Data.Time (Day)
import Numeric (showFFloat)

type Account = String
type Category = String
type Currency = Double

formatDouble :: Int -> Double -> String
formatDouble decimals number = showFFloat (Just decimals) number ""

formatCurrency :: Currency -> String
formatCurrency = formatDouble 2

data Transaction = Transaction {
  destination :: Account,
  amount :: Currency,
  date :: Day,
  category :: Maybe Category
  }

instance Show Transaction where
  show t = intercalate "; " fields
    where fields = ["dest: " ++ show (destination t),
                    "amount: " ++ formatCurrency (amount t),
                    "date: " ++ show (date t),
                    "category: " ++ show (category t)]
