module WebSpider.Base
 (addWebItem,addWebPage,getBytePages,getPageByTitle,myForkPool)
  where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Network.HTTP
import System.IO
import Data.Maybe
import Network.URI
import qualified Data.ByteString as B

import Control.Concurrent

myConn = connectMySQL defaultMySQLConnectInfo {
                        mysqlHost     = "127.0.0.1",
                        mysqlUser     = "sqluser",
                        mysqlPassword = "sqluser"
                     }

addWebItem x = myExe sqlString $ mark2sql x
  where
    sqlString =  "INSERT INTO new_schema.webitem (url,path,filename) values(?,?,?)"

addWebPage x = myExe sqlString $ mark2sql x
  where
    sqlString = "INSERT INTO new_schema.webpage (pagename,pagenum ,pageurl,sittag,keyword) values(?,?,?,?,?)"

getPageByTitle::B.ByteString -> IO [[SqlValue]]
getPageByTitle til =do 
  conn<- myConn
  setChar conn
  quickQuery' conn sqlStr [toSql til]
  where
    sqlStr =  "select * from new_schema.webpage where pagename like ? and sittag <> \"1\""


-- myExeM sqlStr value= myConn>>= (\m -> (m >>= preAndExc) >> (m >>= commit))
--  where
--    preAndExc m = prepare m sqlStr >>= (\y -> executeMany y value) 

setChar conn = do run conn "set names 'utf8'" []

myExe sqlStr value = do
  conn <- myConn
  setChar conn
  stmt <- prepare conn sqlStr
  executeMany stmt value
  commit conn



mark2sql x = map (map toSql ) x 

getBytePages x = 
  (simpleHTTP $  defaultGETRequest_ $ (fromJust . parseURI) x ) >>=getResponseBody::IO B.ByteString

myForkPool f xs count=do
  m <- newMVar []
  myFrokMain f xs count m

myFrokMain f [] count  m = do return ""

myFrokMain f xs count  m = do
  tm <- readMVar m
  myFrokSub f xs count tm m

myFrokSub f xs count tm m 
  | length tm > count = do
    putStrLn $ show tm
    threadDelay 1000000
    myFrokMain f xs count m
  | otherwise = do
    putStrLn $ show tm
    forkOS $ poolFork f (head xs) m
    myFrokMain f (tail xs) count m

    

poolFork f x m = do
  modifyMVar_ m (\y ->liftM (\z -> y ++ [z]) myThreadId)
  f x
  modifyMVar_ m (\y ->liftM (\z ->dropWhile (==z) y ) myThreadId)