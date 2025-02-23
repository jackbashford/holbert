{
module Parse.Parser where

import Debug.Trace(trace)
import Data.Char(isDigit)
import Text.Earley.Mixfix as EPM
import qualified Miso.String as MS
import qualified Editor as E
import qualified Item as I
import qualified SyntaxDecl as SD
import qualified Prop as P
import qualified ProofTree as PT
import qualified Terms as T
import qualified Rule as R
import qualified Heading as H
import qualified Paragraph as PG
import Data.JSString(JSString)
import Editor(Document)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	heading     			{ Heading $$ }
	paragraph   			{ Paragraph $$ }
	"<S>"       			{ SyntaxOpen }
	"</S>"      			{ SyntaxClose }
	syntaxOp    			{ SyntaxOperator $$ }
	syntaxPrec  			{ SyntaxPrecedence $$ }
	syntaxAssoc 			{ SyntaxAssociativity $$ }
	"<R>"       			{ RuleOpen }
	"</R>"      			{ RuleClose }
	"("         			{ ParenOpen }
	")"         			{ ParenClose }
	","         			{ Comma }

%%

Document :: { Document }
Document : Items 			{ reverse $1 }

Items :: { [I.Item] }
Items : {- empty -}   		{ [] }
         | Items Item 		{ $2 : $1 }

Item :: { I.Item }
Item : Heading    			{ I.Heading $1 }

Heading :: { H.Heading }
Heading : heading { H.Heading (fst $1) (MS.ms (snd $1)) }

{
data Token = Heading (Int, String)
           | Paragraph String
		   | SyntaxOpen
		   | SyntaxClose
		   | SyntaxOperator String
		   | SyntaxPrecedence Int
		   | SyntaxAssociativity String
		   | RuleOpen
		   | RuleClose
		   | ParenOpen
		   | ParenClose
		   | Comma
  deriving (Show, Eq)

data LexerState = InHeading Int String | InParagraph String | InSyntax | InRule | Default

lexer :: String -> [Token]
lexer = lexer' Default

lexer' :: LexerState -> String -> [Token]
lexer' _ [] = []
lexer' Default ('<':'H':n:'>':cs) | isDigit n = lexer' (InHeading level "") cs
  where
    level = read [n]
lexer' (InHeading level s) ('<':'/':'H':n:'>':'\n':'\n':cs)
  | isDigit n && level == level' = (Heading (level, s)) : lexer' Default cs
  | otherwise = lexer' Default cs
  where
    level' = read [n]
lexer' (InHeading level s) (c:cs) = lexer' (InHeading level (c:s)) cs
lexer' _ _ = []

parseError = undefined

-- parseDoc needs to use the Earley parser to parse the mixfix operators
-- Its type is `Maybe Document` in case we want to cause a failure at any point here - the actual parsing into a Document by `parse` should never fail, failure would just produce an empty list.
parseDoc :: String -> Maybe Document
parseDoc inp = (trace inp) $ Just $ parse $ lexer inp
}
