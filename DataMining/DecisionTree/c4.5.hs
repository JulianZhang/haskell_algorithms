import Data.Tree

getDiv::Int->Int->Float
getDiv p n= (fromInteger px)/(fromInteger nx)
  where
    px = toInteger p
    nx = toInteger n

getGain xt = (logTemp pt)+(logTemp pf)
  where
    countt = length $ filter (\x -> x) xt
    pt = getDiv countt countAll
    pf = getDiv countf countAll
    countAll =  length $ xt
    countf = countAll - countt
    logTemp x = x*(logBase 2 x)