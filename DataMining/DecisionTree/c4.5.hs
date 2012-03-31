import Data.Tree
import Data.List

getDiv::Int->Int->Float
getDiv p n= (fromInteger px)/(fromInteger nx)
  where
    px = toInteger p
    nx = toInteger n

getEntropy xt = sum $ map (\x -> logTemp x) pList
  where
    groupList = group $ sort xt
    countList = map (\x -> length x) groupList
    countAll =  length xt
    pList = map (\x -> getDiv x countAll) countList
    logTemp x = x*(logBase 2 x)
