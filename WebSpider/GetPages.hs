import Control.Monad
import System.IO
import Data.Maybe
import Text.Regex.PCRE
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import WebSpider.Base

import System.Directory
import System.FilePath.Posix
import Database.HDBC
import Control.Parallel
import Control.Concurrent

fileType =""

catchTile x = x =~ "(?!>).*(?=</title)"::B.ByteString

genPageList::String->String->Int->[String]
genPageList baseUri endUri 1 = [baseUri++(show 1)++endUri] 
genPageList baseUri endUri a = [baseUri++(show a)++endUri] ++ genPageList baseUri endUri (a-1)

myPageList =  genPageList " " " "  8

getPageTitle uri = liftM catchTile $ getBytePages uri

combinStr::(B.ByteString,String)->[B.ByteString]
combinStr (tile,uri) = [ tile,(U.fromString ""),(U.fromString uri),(U.fromString ""),(U.fromString "")]

getLastPage x = getLast pl
  where
    pl = x =~ "(?!>)[0-9]+(?=</a>)"::[[B.ByteString]]
    getLast [] = 1
    getLast xs = read ((U.toString.head.last) xs)::Int

getLink x =  (head.head)pl
  where 
    pl = x =~ ("(?!img src=\")http:[^\r]*" ++ fileType ++"(?=\r\")")::[[B.ByteString]]

getPath x = pl
  where
    pl = x =~(".*/(?=[0-9]+\\." ++ fileType ++")")::B.ByteString


prossPage uri path = do
  tt <- links
  --mapM (\x ->  saveLink path x) tt
  myForkPool (saveLink path) tt 10 
  where
    links = liftM2 (genLinks) linkRoot linkCount
    page =  getBytePages uri
    linkCount = liftM ((6*).getLastPage) page
    linkRoot = liftM (U.toString.getPath.getLink) page

parSaveLink path (x:xs) = liftM (saveLink path) 

saveLink  path link = createDirectoryIfMissing True path >>linkByte >>= B.writeFile (path++linkStr) >> putStrLn link -- 
  where
    linkStr = link =~("(?!/)[^/]+" ++ fileType) ::String
    linkByte = getBytePages link

genLinks path 1 = [path ++ "01." ++ fileType]
genLinks path linkCount = [path ++ (genNum linkCount)++"."  ++ fileType ] ++ genLinks path (linkCount-1)

genNum x 
  | length s ==1 = "0"++s
  | otherwise = s
  where
    s = show x

--main = 11

addPage2db = do
  tiList <- mapM getPageTitle myPageList
  let value = map combinStr $ zip tiList myPageList
  addWebPage value


main =  mapM getPages turi

getPages tl = do 
        comList <-  getPageByTitle tl
        forM_  comList $ \row -> prePage (row!!1) (row!!3) tl  


prePage nameSql urlSql tl  = prossPage url tp        
  where
     path = "/Users/zhangjun/Desktop/code/pic/"
     name = fromSql nameSql
     url = fromSql urlSql
     tp = path ++ "/" ++  U.toString tl ++ "/" ++ name++"/"

turi = [(U.fromString "%%")]

--[]  