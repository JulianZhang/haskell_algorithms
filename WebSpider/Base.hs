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
    --stmt <- prepare myConn "INSERT INTO new_schema.webitem (url,path,filename) values(?,?,?)"         executeMany stmt x

-- myExe sqlStr value= myConn>>=(\y ->prepare y sqlStr)>>= (\y -> executeMany y value) >> myConn >>= commit
myExe sqlStr value = do
  conn <- myConn
  stmt <- prepare conn sqlStr
  executeMany stmt value
  commit conn

addWebPage x = myExe sqlString $ mark2sql x
  Where
    sqlString = "INSERT INTO new_schema.webpage (pagename,pagenum ,pageurl,sittag,keyword) values(?,?,?,?,?)"

mark2sql x = map (map toSql ) x 