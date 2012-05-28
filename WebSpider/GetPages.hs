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

catchTile x = x =~ "(?=>).*(?=</title)"::B.ByteString

genPageList::String->String->Int->[String]
genPageList baseUri endUri 1 = [baseUri++(show 1)++endUri] 
genPageList baseUri endUri a = [baseUri++(show a)++endUri] ++ genPageList baseUri endUri (a-1)

myPageList =  genPageList " " " "  8

getPageTitle uri = liftM catchTile $ getBytePages uri

combinStr::(B.ByteString,String)->[B.ByteString]
combinStr (tile,uri) = [ tile,(U.fromString ""),(U.fromString uri),(U.fromString ""),(U.fromString "")]

getLastPage x =read ((U.toString.head.last) pl)::Int
  where
    pl = x =~ "(?!>)[0-9]+(?=</a>)"::[[B.ByteString]]

getLink x =  (head.head)pl
  where 
    pl = x =~ "(?!img src=\")http:[^\r]*jpg(?=\r\")"::[[B.ByteString]]

getPath x = pl
  where
    pl = x =~".*/(?=[0-9]+\\.jpg)"::B.ByteString

prossPage uri path = do
  tt <- links
  mapM (saveLink path) tt
  where
    links = liftM2 (genLinks) linkRoot linkCount
    page =  getBytePages uri
    linkCount = liftM ((6*).getLastPage) page
    linkRoot = liftM (U.toString.getPath.getLink) page

saveLink  path link = createDirectoryIfMissing True path >>linkByte >>= B.writeFile (path++linkStr) >> putStrLn link -- 
  where
    linkStr = link =~"(?!/)[^/]+jpg"::String
    linkByte = getBytePages link

genLinks path 1 = [path ++ "01.jpg"]
genLinks path linkCount = [path ++ (genNum linkCount)++".jpg"] ++ genLinks path (linkCount-1)

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

getPages tl =  do 
        -- pageList <-  (map getPageByTitle) tl
        --(map getPageByTitle) tl
        comList <-  ( zipWith comPage tl) $ (map getPageByTitle) tl
        --return $ liftM ( zip tl) pageList
        -- tt <-  comPage  comList
        return comList

-- prossAll path tl = do
--  map
--  where
--    pl = getPages tl

comPage::B.ByteString ->IO [[SqlValue]]->IO [(SqlValue,SqlValue,B.ByteString)]
comPage title dbl = liftM (map  combine) dbl
  where
    combine db = ( db!!1, db!!3, title) 

-- test = liftM (getLink)  $ getBytePages   

turi = ["%%"]  