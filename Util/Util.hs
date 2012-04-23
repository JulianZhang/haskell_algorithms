module Util.Util
  where

getDiv::Int->Int->Float
getDiv p n = (fromInteger px)/(fromInteger nx)
  where
    px = toInteger p
    nx = toInteger n

fstGroup x y= (fst x)==(fst y)

fstSort x y
  | (fst x) == (fst y) = EQ
  | (fst x) < (fst y)  = LT
  | otherwise = GT

sndGroup x y= (snd x)==(snd y)

sndSort x y
  | (snd x) == (snd y) = EQ
  | (snd x) < (snd y)  = LT
  | otherwise = GT