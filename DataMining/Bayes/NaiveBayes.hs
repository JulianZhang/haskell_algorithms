module Bayes.NaiveBayes
  where

import Data.List
import Debug.Trace

getAllProb sList = rpList
  where
    resultList = map snd sList
    rpList = getRP resultList

getRP s = map (\x -> (,,) (fst x) (snd x) all) countList
  where
    all = length s
    sortList = sort s
    groupList = group sortList
    countList = map (\x -> (,) (head x) (length x) ) groupList 