import Control.Monad
import System.IO
import Data.Maybe
import Text.Regex.PCRE
import qualified Data.ByteString as B


catchTile x = x =~ "(?=>).*(?=</title)"::B.ByteString

genPageList::String->String->Int->[String]
genPageList baseUri endUri 1 = [baseUri++(show 1)++endUri] 
genPageList baseUri endUri a = [baseUri++(show a)++endUri] ++ genPageList baseUri endUri (a-1)



 


-- test = addWebItem 