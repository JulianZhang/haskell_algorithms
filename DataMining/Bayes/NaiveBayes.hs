module Bayes.NaiveBayes
  where

import Data.List
import Debug.Trace
import Util.Util

getAllProb sList = rpList
  where
    resultList = map snd sList
    rpList = getRP resultList

getRP s = map (\x -> (,) (fst x) (getDiv (snd x) all)) countList
  where
    all = length s
    sortList = sort s
    groupList = group sortList
    countList = map (\x -> (,) (head x) (length x) ) groupList 