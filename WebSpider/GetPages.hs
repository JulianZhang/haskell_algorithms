import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL

import Network.HTTP
import System.IO
import Data.Maybe
import Network.URI
-- import RegExDot.RegEx
-- import RegExDot.RegExOpts
import Text.Regex.PCRE
import Data.Word
import Data.ByteString.UTF8 as U
import qualified Data.ByteString as B

main = do conn <- connectMySQL defaultMySQLConnectInfo {
                        mysqlHost     = "127.0.0.1",
                        mysqlUser     = "sqluser",
                        mysqlPassword = "sqluser"
                     }

          row1s <- setChar conn 
          let x = [[toSql"123", toSql "sf",toSql "ss"],[toSql"333", toSql "sf",toSql "ss"]]
          y <- getBytePages auri
          let ty = catchTile y
          addWebItem conn [[toSql auri,toSql  ty,toSql (U.fromString "中文")]] 
          -- rows <- getWebItem conn
          commit conn
          --forM_ row1s $ \row -> putStrLn $ show row

setChar conn = do run conn "set names 'utf8'" []

getWebItem conn = do quickQuery' conn "set names 'utf8'" []
 
addWebItem conn x = do 
    stmt <- prepare conn "INSERT INTO new_schema.webitem (url,path,filename) values(?,?,?)"
    executeMany stmt x

getPages x =do
      rsp <- Network.HTTP.simpleHTTP (getRequest x)
              -- fetch document and return it (as a 'String'.)
      -- liftM_ catchTile $ getResponseBody rsp
      (getResponseBody rsp)

getBytePages x = 
  (simpleHTTP $  defaultGETRequest_ $ (fromJust . parseURI) x ) >>=getResponseBody::IO ByteString

auri = "http://"

 -- treg x y = (+~) y ( RegExDot.RegExOpts.mkRegEx  x )
testreg x y = x =~ y:: String

catchTile x = x =~ "(?=>).*(?=</title)"::ByteString

fileTxt = liftM (B.take 100) $ B.readFile "/Users/zhangjun/Downloads/zhongjie.txt"

savehttp = (getBytePages auri) >>= B.writeFile "test.html"  

-- test = addWebItem 