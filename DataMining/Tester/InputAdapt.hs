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
  let cs = getAllGain $ take 1000 $ map (\x -> l2t (tran2list x) ) (lines inf)
  return cs