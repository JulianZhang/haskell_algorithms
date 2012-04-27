module Bayes.NaiveBayes
 (getAllProb,getTestProb)
  where

import Data.List
import Debug.Trace
import Util.Util

-- [((flag,flagCount),[[(par1-v1,parCount),(par1-v2,parCount)],[(par2-v1,parCount),(par2-v2,parCount)]])]

getAllProb sList = builCount
  where
    resultList = map snd sList
    nubResultList = nub.sort $ resultList
    nubParList = map (nub.sort) $ transpose $ map fst sList
    rpList = getRP resultList
    groupList = head.group.sort $ resultList
    fList = map (\x -> filter (\y ->  x == (snd y) )  sList ) groupList 
    -- allCount = map (\x -> (filter (\y -> x == (snd y) ) sList) ) nubResultList 
    allCount = map (\x -> getParListCount (filter (\y -> x == (snd y) ) sList)   nubParList ) nubResultList
    builCount = zip (map (\x -> (,) x (length (filter (\y -> x==y) resultList )) ) nubResultList) allCount 
    
getParListCount fl npl =  zipWith getParCount (transpose (map fst fl) ) npl

getParCount allPar nPar = map (\x -> (,) x  (length(elemIndices x allPar ))) nPar

getRP s = map (\x -> (,) (fst x) (getDiv (snd x) all)) countList
  where
    all = length s
    sortList = sort s
    groupList = group sortList
    countList = map (\x -> (,) (head x) (length x) ) groupList

getTestProb builCount testData = maxProb 
  where
    all = sum $ map (snd.fst) builCount
    allProb = map (getFlagProb testData all ) builCount
    maxProb = fst $ maximumBy  sndSort  allProb

getFlagProb td all flagCounts = (,) flag  (parProb*flagProb) 
  where
    flag = (fst.fst) flagCounts
    flagCount = (snd.fst) flagCounts
    parLists = snd flagCounts
    parCounts = zipWith searchParCount td parLists 
    flagProb = getDiv flagCount all 
    parProb = product $ map (\x -> getDiv x flagCount ) parCounts

searchParCount::String->[(String,Int)]->Int
searchParCount testPar [] = 1
searchParCount testPar (x:xs)
  | (fst x) == testPar = ((snd x) +1)
  | otherwise =  searchParCount testPar xs 
 