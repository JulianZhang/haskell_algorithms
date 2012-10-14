import Data.List
import Data.Tree

apriori::Ord a =>[[a]]->Int->[([a],Int)]
apriori llc min = apriAll llc min st st
  where
    st = genL1 llc min
    sortll = (map sort llc)
    genL1 llc min = map (\x -> (,) [(head x)] (length x) )(filter (\x -> (length x)>min) (group allList))
      where
        allList = sort (concat llc)
    
aprjoin step1 step2= map (\y -> (map (\x ->aprapend x y) (getFist step1)) ) (map (\x -> [(last x)]) (getFist step2))
getFist lx = map (fst) lx

aprapend lx ly 
  | (isInfixOf ly lx) = []
  | otherwise = sort (lx++ly)

getCountMap llc step = map (\x ->(,) x (getCount llc x)) step
  where
   getCount llc la = length.filter (\z ->z) $ map (\x -> all (\y -> elem y x) la) llc 

-- commit test
apriAll llc min allStep curStep 
  |(not.null) curStep = apriAll llc min (allStep++newCur) newCur
  |otherwise = allStep
  where 
    newCur = getCountMap llc $ filterNew  curStep
    jointemp step =  aprjoin step step
    filterNew step = filter (not.null)  $ foldl union [] (jointemp step)

-- fptree entry
-- fpTree llc min = 

testLLC = [["a","b","c"],["b","d","a"],["c","a","d"]]
treeRoot = Node ("",99) []
testTree = Node ("",99) [Node ("b",5) [],Node ("d",4) []]

buildFPTree llc tree [] = tree

-- buildFPTree llc tree itemLists = 

mainFPTree llc = map (\x -> ) sList
  where
    sList = getSortList llc

sndSort x y
  | (snd x) == (snd y) = EQ
  | (snd x) < (snd y)  = GT
  | otherwise = LT
   
getSortList llc = sortBy sndSort countList
  where
    allList = group $  sort $ concat llc
    countList =  map (\x -> (head x,length x)) allList
    

addTreeNode trace addNodeList myTree
  | null traceItem = Node rootValue (nextLevel)
  | otherwise = Node rootValue (nextLevel ++ [Node traceNode []])
  where
    nextLevel = map addNewTreeNode (subForest myTree)
    addNewTreeNode = addTreeNode newTrace addNodeList
    newTrace = trace ++ [rootValue]
    rootValue = (fst.rootLabel) myTree
    traceItem = filter (\x -> listEq newTrace (fst x)) addNodeList
    traceNode = (snd.head) traceItem

listEq la lb
  | length la ==length lb = and $ zipWith (==) la lb
  | otherwise = False

--listTreeNode::Tree a->[a]->[[a]]
listTreeNode trace myTree  = nextLevel ++ [newTrace]
  where
    rootValue = (fst.rootLabel) myTree
    nextLevel = concat $ map (listTreeNode newTrace) (subForest myTree)
    newTrace = trace ++[rootValue]

findPosition node llc treeList
 |null (fst filterLLC) = nextCall
 |otherwise = [(needFind,length (fst filterLLC))]
  where
    curTl = head treeList
    nextTl = tail treeList
    needFind = curTl ++ node
    filterLLC = partition (\x -> and (map (\y-> elem y x) needFind) ) llc
    nextCall = findPosition node (snd filterLLC) nextTl
    
    
-- myFilter  y =  

getItemLists llc = foldr getItemList []  llc

getItemList inList itemList = foldr addList itemList inList

addList  item itemList 
  | [] == itemIndexs = itemList ++ [(item,1)]
  | otherwise = newList
  where 
    itemIndexs = findIndices  (\x -> item == fst x) itemList
    itemIndex = head itemIndexs
    itemPar = itemList!!itemIndex
    newList = (take (itemIndex ) itemList) ++ (drop (itemIndex + 1) itemList ) ++ [(fst itemPar,(snd itemPar)+1)]
