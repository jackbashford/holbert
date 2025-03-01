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
     | SyntaxDecl           { trace (show $1) $ I.SyntaxDecl $1 }

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
SyntaxItem : syntaxTok syntaxTok syntaxTok { trace ("Syntax items: " ++ show ($1, $2, $3)) $ constructSyntaxItem $1 $2 $3 }

{
data Token = Heading (Int, String)
           | Paragraph String
           | SyntaxOpen
           | SyntaxClose
           | SyntaxToken String
           | RuleOpen
           | RuleClose
           | ParenOpen
           | ParenClose
           | Comma
  deriving (Show, Eq)

constructSyntaxItem :: String -> String -> String -> Maybe (Int, MS.MisoString, EPM.Associativity)
constructSyntaxItem prec op assoc | (Just p', Just a') <- (prec', assoc') = Just (p', op', a')
                                  | otherwise = Nothing
  where
    prec' | all isDigit prec = trace "all digits" $ Just $ read prec
          | otherwise        = trace "nope" $ Nothing
    op' = MS.ms op
    assoc' = case assoc of
      "left" -> Just EPM.LeftAssoc
      "right" -> Just EPM.RightAssoc
      "no" -> Just EPM.NonAssoc
      _ -> Nothing

data LexerState = InHeading Int String | InParagraph String | InSyntax | InRule | Default

lexer :: String -> [Token]
lexer = lexer' Default

lexer' :: LexerState -> String -> [Token]
lexer' _ [] = []

lexer' Default (c:cs) | isSpace c = lexer' Default cs

-- Headings
lexer' Default ('<':'H':n:'>':cs) | isDigit n = lexer' (InHeading level "") cs
  where
    level = read [n]
lexer' (InHeading level s) ('<':'/':'H':n:'>':'\n':'\n':cs)
  | isDigit n && level == level' = (Heading (level, reverse s)) : lexer' Default cs
  | otherwise = lexer' Default cs
  where
    level' = read [n]
lexer' (InHeading level s) (c:cs) = lexer' (InHeading level (c:s)) cs

-- Paragraphs
lexer' Default cs | Just cs' <- stripPrefix "<P>" cs = lexer' (InParagraph "") cs'
lexer' (InParagraph s) cs | Just cs' <- stripPrefix "</P>" cs = (Paragraph (reverse s)) : lexer' Default cs'
lexer' (InParagraph s) (c:cs) = lexer' (InParagraph (c:s)) cs

-- Syntax Declarations
lexer' Default cs | Just cs' <- stripPrefix "<S>\n" cs = SyntaxOpen : lexer' InSyntax cs'
lexer' InSyntax cs | Just cs' <- stripPrefix "</S>" cs = SyntaxClose : lexer' Default cs'
lexer' InSyntax cs = (SyntaxToken s) : lexer' InSyntax (dropWhile (== '\n') cs')
  where
    -- Drops newlines
    (s, cs') = span (/= '\n') cs

-- Rules
lexer' Default cs | Just cs' <- stripPrefix "<R>" cs = lexer' InRule cs'
lexer' InRule cs | Just cs' <- stripPrefix "</R>" cs = lexer' Default cs'
lexer' InRule (c:cs) = lexer' InRule cs

lexer' _ cs = trace cs $ [Paragraph cs]

parseError :: [Token] -> a
parseError tks = error $ "Parse error! Tks: " ++ show tks

-- parseDoc needs to use the Earley parser to parse the mixfix operators
-- Its type is `Maybe Document` in case we want to cause a failure at any point here - the actual parsing into a Document by `parse` should never fail, failure should just produce an empty list.
parseDoc :: String -> Maybe Document
parseDoc inp = let ts = lexer inp in (trace $ show ts) $ Just $ parse $ lexer inp
}
