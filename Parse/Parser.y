{
{-# LANGUAGE OverloadedStrings #-}
module Parse.Parser where

import Debug.Trace(trace)
import Data.Char(isDigit, isSpace)
import Data.List(stripPrefix, span)
import Data.Maybe(catMaybes)
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
import Parse.Lexer
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
    syntaxTok               { SyntaxToken $$ }
	"<R>"       			{ RuleOpen }
	"</R>"      			{ RuleClose }
	"("         			{ ParenOpen }
	")"         			{ ParenClose }
	","         			{ Comma }

%%

Document :: { Document }
Document : Items            { reverse $1 }

Items :: { [I.Item] }
Items : Items Item          { $2 : $1 }
      | Item                { [$1] }

Item :: { I.Item }
Item : Heading              { I.Heading $1 }
     | Paragraph            { I.Paragraph $1 }
     | SyntaxDecl           { I.SyntaxDecl $1 }

Heading :: { H.Heading }
Heading : heading { H.Heading (fst $1) (MS.ms (snd $1)) }

Paragraph :: { PG.Paragraph }
Paragraph : paragraph { PG.Paragraph (MS.ms $1) }

SyntaxDecl :: { SD.SyntaxDecl }
SyntaxDecl : "<S>" SyntaxItems "</S>" { (SD.SyntaxDecl . reverse . catMaybes) $2 }

SyntaxItems :: { [Maybe (Int, MS.MisoString, EPM.Associativity)] }
SyntaxItems : SyntaxItem               { [$1] }
            | SyntaxItems SyntaxItem   { $2 : $1 }


SyntaxItem :: { Maybe (Int, MS.MisoString, EPM.Associativity) }
SyntaxItem : syntaxTok syntaxTok syntaxTok { constructSyntaxItem $1 $2 $3 }

{
parseError :: [Token] -> a
parseError tks = error $ "Parse error! Tks: " ++ show tks

-- parseDoc needs to use the Earley parser to parse the mixfix operators
-- Its type is `Maybe Document` in case we want to cause a failure at any point here - the actual parsing into a Document by `parse` should never fail, failure should just produce an empty list.
parseDoc :: String -> Maybe Document
parseDoc inp = let ts = lexer inp in {- (trace $ "Input: " ++ inp ++ ", toks: " ++ show ts) $ -} Just $ parse $ lexer inp
}
