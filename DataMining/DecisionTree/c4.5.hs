module DecisionTree.C4_5
  where

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

-- getAllGain::Ord a =>[([a],b)]->[[a]]
getAllGain sList = zipWith (/) gainList (getSplitInfox pList all)  
  where
    propsList = filter (\x ->(not.null) x) $ getProps $ map (\x -> fst x) sList
    valueList = map (\x -> snd x) sList
    zipList = map (\x -> zip x valueList) propsList
    pList = map (\x -> (groupBy fstGroup) (sortBy fstSort x) ) zipList
    all = sum $ map (\x -> length x) $ head pList
    gainList = map (\x -> (sum x)/(toFloat all)) baseGainList 
    baseGainList = map (\x -> map (\y -> propGain y ) x) pList
    

getSplitInfox pList all = map (\x ->sum (getOneSplitInfo  x all )) pList
    
getOneSplitInfo vlist all = map (\y -> logTemp (getDiv (length y) all) ) vlist
    
fstGroup x y= (fst x)==(fst y)

fstSort x y
  | (fst x) == (fst y) = EQ
  | (fst x) < (fst y)  = LT
  | otherwise = GT

propGain sl = ((toFloat.length) sl) * (getEntropy (map (\x -> snd x) sl ))

toFloat x = (fromInteger.toInteger) x :: Float

getProps::[[a]]->[[a]]
getProps pl 
      | ((length.head) pl )== 0 = [[]]
      | otherwise = [map (\x -> head x) pl]  ++ (getProps (map (\x -> tail x) pl) )  