module Experiments where

data Bla = Bla {
  a :: Double,
  b :: Double -> Double
}

transformBla :: (Double -> Double) -> Bla -> Bla
transformBla fn bla = Bla {
  a = (a bla),
  b = (fn . (b bla))
  }
