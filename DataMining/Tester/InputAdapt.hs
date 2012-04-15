import DecisionTree.C4_5

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
test = do
  inf <- myReadFile dataPath adultData
  let cs = take 5000 $ map (\x -> l2t (tran2list x) ) (lines inf)
  let gs = maxGainIndex $ getAllGain cs
  let nl = nub_by 11 $ map (\x -> fst x ) cs
  let all = listStep cs (-1)
  return all

listStep::[([String],String)]->Int->[Int]
listStep cs i
  | ((maximum.getAllGain) cs) == 0 = [i]
  | otherwise = concat $ map (\x -> listStep x maxI) vList  
  where 
    maxI = maxGainIndex $ getAllGain cs
    nList = nub_by maxI $ map (\x -> fst x ) cs
    vList = map (\x -> filterBy x maxI  cs) nList