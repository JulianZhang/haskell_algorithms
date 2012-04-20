import DecisionTree.C4_5
import Data.Tree
import Data.List
import Debug.Trace

dataPath = "/Users/zhangjun/Desktop/code/haskell_algorithms/dataset/"

adultData = "adult/adult.data.txt"
adultTest = "adult/adult.test.txt"

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
  let cs = take i $ map (\x -> l2t (tran2list x) ) (lines inf)
  let fcs = filter (\x -> (not.null) (snd x) ) cs
  let all = listStep fcs (-1) "tt" 0  
  return all

allTree = buildTree 40

testFlag i = do 
  inf <- myReadFile dataPath adultData
  let cs = take i $ map (\x -> l2t (tran2list x) ) (lines inf)
  let tData = (fst.last) cs
  myTree <- allTree
  let tFlag = searchResult myTree (trace ("input list" ++ (show tData))  tData)
  return $ length tFlag

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

