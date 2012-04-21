import Data.List

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

