module Bayes.NaiveBayes
  where

import Data.List
import Debug.Trace
import Util.Util

getAllProb sList = allCount
  where
    resultList = map snd sList
    nubResultList = nub.sort $ resultList
    nubParList = map (nub.sort) $ transpose $ map fst sList
    rpList = getRP resultList
    groupList = head.group.sort $ resultList
    fList = map (\x -> filter (\y ->  x == (snd y) )  sList ) groupList 
    -- allCount = map (\x -> (filter (\y -> x == (snd y) ) sList) ) nubResultList 
    allCount = map (\x -> getParListCount (filter (\y -> x == (snd y) ) sList)   nubParList ) nubResultList 
    
getParListCount fl npl =  zipWith getParCount (transpose (map fst fl) ) npl

getParCount allPar nPar = map (\x -> (,) x (elemIndices x allPar )) nPar

getRP s = map (\x -> (,) (fst x) (getDiv (snd x) all)) countList
  where
    all = length s
    sortList = sort s
    groupList = group sortList
    countList = map (\x -> (,) (head x) (length x) ) groupList

 