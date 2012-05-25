import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL

import Network.HTTP
import System.IO
import Data.Maybe
import Network.URI
import Text.Regex.PCRE
import Data.Word
import Data.ByteString.UTF8 as U
import qualified Data.ByteString as B


getPages x =do
      rsp <- Network.HTTP.simpleHTTP (getRequest x)
              -- fetch document and return it (as a 'String'.)
      -- liftM_ catchTile $ getResponseBody rsp
      (getResponseBody rsp)


 -- treg x y = (+~) y ( RegExDot.RegExOpts.mkRegEx  x )
testreg x y = x =~ y:: String

catchTile x = x =~ "(?=>).*(?=</title)"::ByteString

fileTxt = liftM (B.take 100) $ B.readFile "/Users/zhangjun/Downloads/zhongjie.txt"

savehttp = (getBytePages auri) >>= B.writeFile "test.html"  

-- test = addWebItem 