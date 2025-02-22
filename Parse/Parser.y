{
module Parse.Parser where

import qualified Editor as E
import qualified Item as I
import qualified Prop as P
import qualified ProofTree as PT
import qualified Terms as T
import qualified Rule as R
import qualified Heading as H
import qualified Paragraph as PG
import Data.JSString(JSString)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	x { TokenX }

%%

X : x { XItem }

{
data ParseResult = XItem

data Token = TokenX

lexer :: String -> [Token]
lexer = map (const TokenX)

parseError = undefined

parseDoc = undefined
}
