module WebSpider.Base
 (addWebItem,addWebPage,getBytePages)
  where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Network.HTTP
import System.IO
import Data.Maybe
import Network.URI
import qualified Data.ByteString as B

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