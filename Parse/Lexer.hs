module Parse.Lexer where

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
    prec' | all isDigit prec && not (null prec) = Just $ read prec
          | otherwise                           = Nothing
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
lexer' Default ('<':'H':n:'>':cs) | isDigit n = {- trace ("Enter heading of level: " ++ show n ++ "\n") $ -} lexer' (InHeading level "") cs
  where
    level = read [n]
lexer' (InHeading level s) ('<':'/':'H':n:'>':cs)
  | isDigit n && level == level' = {- trace ("Consumed heading of level: " ++ show n ++ "\n") $ -} (Heading (level, reverse s)) : lexer' Default cs
  | otherwise = {- trace "Fail to read heading" $ -} lexer' Default cs
  where
    level' = read [n]
-- lexer' (InHeading level s) ('\\':'<':cs) = lexer' (InHeading level ('<':s)) cs
lexer' (InHeading level s) (c:cs) = lexer' (InHeading level (c:s)) cs

-- Paragraphs
lexer' Default cs | Just cs' <- stripPrefix "<P>" cs = lexer' (InParagraph "") cs'
lexer' (InParagraph s) cs | Just cs' <- stripPrefix "</P>" cs = (Paragraph (reverse s)) : lexer' Default cs'
-- lexer' (InParagraph s) ('\\':'<':cs) = lexer' (InParagraph ('<':s)) cs
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

-- TODO remove
lexer' _ cs = [Paragraph cs]

