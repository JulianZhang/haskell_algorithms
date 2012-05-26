import Control.Monad
import System.IO
import Data.Maybe
import Text.Regex.PCRE
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import WebSpider.Base


catchTile x = x =~ "(?=>).*(?=</title)"::B.ByteString

genPageList::String->String->Int->[String]
genPageList baseUri endUri 1 = [baseUri++(show 1)++endUri] 
genPageList baseUri endUri a = [baseUri++(show a)++endUri] ++ genPageList baseUri endUri (a-1)

myPageList =  genPageList " " " "  8

getPageTitle uri = liftM catchTile $ getBytePages uri

combinStr::(B.ByteString,String)->[B.ByteString]
combinStr (tile,uri) = [ tile,(U.fromString ""),(U.fromString uri),(U.fromString ""),(U.fromString "")]



main = do
  tiList <- mapM getPageTitle myPageList
  let value = map combinStr $ zip tiList myPageList
  addWebPage value



-- test = addWebItem 