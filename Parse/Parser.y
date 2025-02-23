{
module Parse.Parser where

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
	heading     			{ Heading $$ $$ }
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
>	 | Paragraph  			{ I.Paragraph $1 }
>	 | SyntaxDecl 			{ I.SyntaxDecl $1 }
>	 | Rule       			{ I.Rule $1 }

Heading :: { H.Heading }
Heading : heading { H.Heading $1 (MS.ms $2) }

{
data Token = Heading Int String
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

data LexerState = InHeading Int | InParagraph | InSyntax | InRule | Default

lexer :: String -> [Token]
lexer = lexer' Default

lexer' :: LexerState -> String -> [Token]
lexer' _ [] = []
lexer' Default ('<':'H':n:'>':cs) | isDigit n = lexer' (InHeading level) cs

parseError = undefined

parseDoc = undefined
}
