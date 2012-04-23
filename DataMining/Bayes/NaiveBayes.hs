module Bayes.NaiveBayes
  where

import Data.List
import Debug.Trace
import Util.Util

getAllProb sList = fList
  where
    resultList = map snd sList
    rpList = getRP resultList
    groupList = head.group.sort $ resultList
    fList = map (\x -> filter (\y ->  x == (snd y) )  sList ) groupList 
    


getRP s = map (\x -> (,) (fst x) (getDiv (snd x) all)) countList
  where
    all = length s
    sortList = sort s
    groupList = group sortList
    countList = map (\x -> (,) (head x) (length x) ) groupList

 