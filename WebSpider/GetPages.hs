import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL

import Network.HTTP
import System.IO
import Data.Maybe
import Network.URI
main = do conn <- connectMySQL defaultMySQLConnectInfo {
                        mysqlHost     = "127.0.0.1",
                        mysqlUser     = "sqluser",
                        mysqlPassword = "sqluser"
                     }

          row1s <- quickQuery' conn "SELECT 1 + 1" []
          let x = [[toSql"123", toSql "sf",toSql "ss"],[toSql"333", toSql "sf",toSql "ss"]]
          addWebItem conn x 
          rows <- getWebItem conn
          forM_ rows $ \row -> putStrLn $ show row

getWebItem conn = do quickQuery' conn "select * from new_schema.webitem" []
 
addWebItem conn x = do 
    stmt <- prepare conn "INSERT INTO new_schema.webitem (url,path,filename) values(?,?,?)"
    executeMany stmt x