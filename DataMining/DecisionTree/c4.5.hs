module DecisionTree.C4_5
  (listStep)
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

maxGainIndex s = myHead $ findIndices (\x -> x ==(maximum s)) s
  where
    myHead [] = 0
    myHead xs = head xs

filterBy v i = filter (\x -> ((fst x)!!i)==v )

nub_by i ls = nub $ map (\x -> x!!i ) ls

-- listStep::Ord a => [([a],a)]->Int->[(Int,Int)]
listStep cs i v 
  | ((maximum.getAllGain) cs) == 0 = Node (i,v) []
  -- can't improve | isAlltheSame cs = [(i,countI)]
  | 1 == countI = Node (i,v) [] 
  | otherwise = Node (i,v) $ map (\x -> listStep (snd x) maxI (fst x)) vList  
  where 
    maxI = maxGainIndex $ getAllGain cs
    nList = nub_by maxI $ map (\x -> fst x ) cs
    vList = map (\x -> (,) x (filterBy x maxI  cs)) nList
    countI = length cs

isAlltheSame cs
  | 1 == length distinList = True
  | otherwise = False
  where
    tagList = map (\x -> snd x) cs
    distinList = nub tagList 