module Util.Util
  where

getDiv::Int->Int->Float
getDiv p n = (fromInteger px)/(fromInteger nx)
  where
    px = toInteger p
    nx = toInteger n
