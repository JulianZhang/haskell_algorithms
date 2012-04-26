import DecisionTree.C4_5
import Bayes.NaiveBayes
import Data.Tree
import Data.List
import Debug.Trace

dataPath = "/Users/zhangjun/Desktop/code/haskell_algorithms/dataset/"

adultData = "adult/adult.data.txt"
adultTest = "adult/adult.test1.txt"

myReadFile p f = readFile $ p ++ f

checkStr s = notElem ',' s

tran2list s 
  | checkStr s = [s]
  | otherwise = [(fst ot)]++(tran2list (tail (snd ot)) ) 
  where
    ot = span (\x -> x /= ',') s

l2t s = (,) (init s) (last s)

-- test = lines $ myReadFile dataPath adultData 
buildTree i = do
  inf <- myReadFile dataPath adultData
  let cs = take i $ getFiltedList inf
  let all = listStep cs (-1) "tt" 0  
  return all

getFiltedList strInput = filter (\x -> (not.null) (snd x) ) listAll
  where
    listAll = map (\x -> l2t (tran2list x) ) (lines strInput)

allTree = buildTree 40

testFlag i j = do 
  inf <- myReadFile dataPath adultTest
  let cs = take i $ getFiltedList inf
  let tData = map fst cs
  let tCheck = map (filter (\x -> x/='.') ) $ map snd cs -- 
  myTree <- buildTree j
  let tFlag  = map (\x -> searchResult myTree  x) tData
  let final = zip tCheck tFlag
  return final

checkAll i j = do
  fList <- testFlag i j
  let rList = map checkI fList
  let all = length rList
  let right = length $ filter (\x -> x ) rList
  return (all,right)  

checkI x
  |null tFlag = False
  |hFlag == tData = True
  |otherwise = False
  where
    tData = fst x
    tFlag = snd x
    hFlag = head tFlag

test2 i = do
  inf <- myReadFile dataPath adultData
  let cs = take i $ map (\x -> l2t (tran2list x) ) (lines inf)
  -- let gs = maxGainIndex $ getAllGain cs
  -- let nl = nub_by 11 $ map (\x -> fst x ) cs
  let all = listStep cs (-1) "tt" 0
  let count = length $ map (\x -> l2t (tran2list x) ) (lines inf)
  let errCs = getAllGain $ filterList cs  [(8," White"), (13," United-States"), (9," Male"),(10," 0"), (5," Married-civ-spouse"), (7," Husband"), (1," Private"),(3," HS-grad"), (12," 40"),(6," Craft-repair"), (0,"49")]
  let maxErr = (last cs)  
  return maxErr

testBayes i = do
  inf <- myReadFile dataPath adultData
  let cs = take i $ getFiltedList inf
  let re = getAllProb cs
  return re

testBayesFlag i j = do 
  inf <- myReadFile dataPath adultTest
  let cs = take i $ getFiltedList inf
  let tData = map fst cs
  let tCheck = map (filter (\x -> x/='.') ) $ map snd cs -- 
  pcs <- testBayes j
  let tFlag  = map (\x -> getTestProb pcs  x) tData
  let final = zip tCheck tFlag
  let all = length final
  let lenTrue = length $ filter (\x -> (fst x)==(snd x)) final
  return $ (,) lenTrue all

-- tTree::[a]->Tree a
tTree i
  | 1 == li = Node hi []
  | otherwise = Node hi  [(tTree.tail) i ] 
  where
    li = length i
    hi = head i
    
maxGainIndex1 s = myHead $ findIndices (\x -> x ==(maximum s)) s
  where
    myHead [] = 0
    myHead xs = head xs

