module DecisionTree.C4_5
  (getAllGain,listStep,filterList,searchResult)
  where

import Data.Tree
import Data.List
import Debug.Trace
import Util.Util


getEntropy xt = sum $ map (\x -> logTemp x) pList
  where
    groupList = group $ sort xt
    countList = map (\x -> length x) groupList
    countAll =  length xt
    pList = map (\x -> getDiv x countAll) countList

logTemp x = x*(logBase 2 x)

-- getAllGain::Ord a =>[([a],b)]->[[a]]
getAllGain sList = zipWith (/) gainList  (getSplitInfox pList all)  
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
    
propGain sl = ((toFloat.length) sl) * (getEntropy (map (\x -> snd x) sl ))

toFloat x = (fromInteger.toInteger) x :: Float

getProps:: [[String]]->[[String]]
getProps pl 
  | ((length.head) pl )== 0 = [[]]
  | otherwise = [map (\x -> tHead x) pl]  ++ (getProps (map (\x -> tail x) pl) )  -- (last.reverse)
  where
    tHead [] = head $ trace (show pl) ["tt"]
    tHead s =  head s

testHead s = trace "testHead input "

maxGainIndex s 
  | null fs = myHead $ findIndices (\x -> x ==0) (trace "ept" s)
  | otherwise = myHead $ findIndices (\x -> x ==(maxTag)) s
  where
    -- myHead [] = (trace "empty s" 0)
    myHead xs 
      | null xs = (trace "empty s" 0)
      | otherwise = head xs
    maxTag = maximum fs
    fs =  filter (\x -> (getDiv 1 0)  /= x) s

filterBy v i = filter (\x -> ((fst x)!!i)==v )

nub_by i ls = nub $ map (\x -> x!!i ) ls

-- listStep::Ord a => [([a],a)]->Int->[(Int,Int)]
listStep cs i v level 
  | isAlltheSame cs = Node (i,v,(groupValueList cs),level) []
  | ((maximum.getAllGain) cs) == 0 = Node (i,v,(groupValueList cs),level) []
  | 1 == countI = Node (i,v,(groupValueList cs),level) []
  | 15 < level = Node (i,v,(groupValueList (trace "level up than 15 " cs)),level) []
  | otherwise = Node (i,v,"tt",level) $ map (\x -> listStep (snd x) maxI (fst x) (level+1)) vList  
  where 
    maxI = maxGainIndex $ getAllGain cs
    nList = nub_by maxI $ map (\x -> fst x ) cs
    vList = map (\x -> (,) x (filterBy x maxI  cs)) nList
    countI = length cs

searchResult treeNode testData
  | -1 == idx = nextLevel
  | tValue /= value = []
  | flag /= "tt" = [flag]
  | otherwise = nextLevel
  where
    rv =  rootLabel treeNode
    idx = fst $ getIndexValue rv
    value = snd $ getIndexValue rv
    tValue = testData !! idx
    flag = getFlag  rv
    subTree = subForest treeNode
    nextLevel = concat $ map (\x -> searchResult x testData ) subTree

getFlag (_,_,a,_) = a

getIndexValue (a,b,_,_) = (a,b)


----------------- test code
groupValueList sList = head nubList
  where
    valueList = map (\x -> snd x) sList
    nubList = nub valueList

isAlltheSame cs
  | 1 == length distinList = True
  | otherwise = False
  where
    tagList = map (\x -> snd x) cs
    distinList = nub tagList 

filterList cs [] = cs
filterList cs ls = filterList fs tl
  where
    tl = tail ls
    hl = head ls
    fs = filterBy (snd hl) (fst hl) cs