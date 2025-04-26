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
import qualified StringRep as SR

data Token = Heading (Int, String)
           | Paragraph String
           | SyntaxOpen
           | SyntaxClose
           | SyntaxToken String
           | RuleOpen
           | RuleClose
           | Kind R.RuleType
           | Style PT.ProofStyle
           | Subtitle String
           | Name String
           | Vars [String]
           | TermString String
           | RuleItemOpen
           | RuleItemClose
           | SubtreesOpen
           | SubtreesClose
           | SubtreeOpen
           | SubtreeClose
           | PremiseOpen
           | PremiseClose
           | ProofOpen
           | ProofClose
           | GoalOpen
           | GoalClose
           | RuleRefOpen
           | RuleRefClose
           | ConclusionOpen
           | ConclusionClose
           | Counter Int
           | RRDefn String
           | RRLocal Int
           | RRCases (String, Int)
           | RRInduction (String, Int)
           | RRRefl
           | RRTrans
           | RRInject
           | RRDistinctOpen
           | RRDistinctClose
           | RRElimOpen
           | RRElimClose
           | RRRewriteOpen
           | RRRewriteClose
           | RRFlipped
           | RRLHS
           | RRRHS
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

makeKindToken :: String -> Token
makeKindToken "Axiom" = Kind R.Axiom
makeKindToken "Theorem" = Kind R.Theorem
makeKindToken "Inductive" = Kind R.Inductive
makeKindToken _ = error "Unknown rule kind!"

makeStyleToken :: String -> Token
makeStyleToken "Tree" = Style PT.Tree
makeStyleToken "Calc" = Style PT.Calc
makeStyleToken "Prose" = Style PT.Prose
makeStyleToken "Abbr" = Style PT.Abbr
makeStyleToken _ = error "Unknown style!"

data LexerState = InHeading Int String
                | InParagraph String
                | InSyntax
                | InRule
                | InVars [String]
                | InName String
                | InKind String
                | InTermStr String
                | InCounter String
                | InStyle String
                | InSubtitle String
                | InProp
                | InRuleRef (Maybe PartialRuleRef)
                | Default

data PartialRuleRef = Defn String
                    | Local String
                    | Cases ConstructorType String (Maybe String)

data ConstructorType = C | I

lexer :: String -> [Token]
lexer = lexer' Default

lexer' :: LexerState -> String -> [Token]
lexer' _ [] = []

lexer' Default (c:cs) | isSpace c = lexer' Default cs

-- Headings
lexer' Default ('<':'H':n:'>':cs) | isDigit n = lexer' (InHeading level "") cs
  where
    level = read [n]
lexer' (InHeading level s) ('<':'/':'H':n:'>':cs)
  | isDigit n && level == level' = (Heading (level, reverse s)) : lexer' Default cs
  | otherwise = lexer' Default cs
  where
    level' = read [n]
lexer' (InHeading level s) ('\\':'<':cs) = lexer' (InHeading level ('<':s)) cs -- Escapes
lexer' (InHeading level s) (c:cs) = lexer' (InHeading level (c:s)) cs

-- Paragraphs
lexer' Default cs | Just cs' <- stripPrefix "<P>" cs = lexer' (InParagraph "") cs'
lexer' (InParagraph s) cs | Just cs' <- stripPrefix "</P>" cs = (Paragraph (reverse s)) : lexer' Default cs'
lexer' (InParagraph s) ('\\':'<':cs) = lexer' (InParagraph ('<':s)) cs -- Escapes
lexer' (InParagraph s) (c:cs) = lexer' (InParagraph (c:s)) cs

-- Syntax Declarations
lexer' Default cs | Just cs' <- stripPrefix "<S>\n" cs = SyntaxOpen : lexer' InSyntax cs'
lexer' InSyntax cs | Just cs' <- stripPrefix "</S>" cs = SyntaxClose : lexer' Default cs'
lexer' InSyntax cs = (SyntaxToken s) : lexer' InSyntax (dropWhile (== '\n') cs')
  where
    -- Drops newlines
    (s, cs') = span (/= '\n') cs

-- Rules
lexer' Default cs | Just cs' <- stripPrefix "<R>" cs = RuleOpen : lexer' InRule cs'
lexer' InRule cs | Just cs' <- stripPrefix "</R>" cs = RuleClose : lexer' Default cs'

-- Rule 'kinds' (rule types) TODO rename
lexer' InRule cs | Just cs' <- stripPrefix "<KIND>" cs = lexer' (InKind "") cs'
lexer' (InKind s) cs | Just cs' <- stripPrefix "</KIND>" cs = (makeKindToken (reverse s)) : lexer' InRule cs'
lexer' (InKind s) (c:cs)
  | isSpace c = lexer' (InKind s) cs
  | otherwise = lexer' (InKind (c:s)) cs

-- unparsed mixfix strings
lexer' InRule ('`':'`':cs) = lexer' (InTermStr "") cs
lexer' (InTermStr s) ('`':'`':cs) = TermString (reverse s) : lexer' InRule cs
lexer' (InTermStr s) (c:cs) = lexer' (InTermStr (c:s)) cs

-- Variables
lexer' InRule cs | Just cs' <- stripPrefix "<VARS>" cs = lexer' (InVars []) cs'
lexer' (InVars vs) cs | Just cs' <- stripPrefix "</VARS>" cs = Vars (reverse vs) : lexer' InRule cs'
lexer' (InVars vs) (c:cs) | isSpace c = lexer' (InVars vs) cs
lexer' (InVars vs) cs = lexer' (InVars (v:vs)) cs'
  where
    (v, cs') = span (not . isSpace) cs

-- Names of rules
lexer' InRule cs | Just cs' <- stripPrefix "<NAME>" cs = lexer' (InName []) cs'
lexer' (InName s) cs | Just cs' <- stripPrefix "</NAME>" cs = Name (reverse s) : lexer' InRule cs'
lexer' (InName s) (c:cs) = lexer' (InName (c:s)) cs

-- Proof styles
lexer' InRule cs | Just cs' <- stripPrefix "<STYLE>" cs = lexer' (InStyle []) cs'
lexer' (InStyle s) cs | Just cs' <- stripPrefix "</STYLE>" cs = (makeStyleToken (reverse s)) : lexer' InRule cs'
lexer' (InStyle s) (c:cs) = lexer' (InStyle (c:s)) cs

-- Proof subtitles
lexer' InRule cs | Just cs' <- stripPrefix "<SUBTITLE>" cs = lexer' (InSubtitle []) cs'
lexer' (InSubtitle s) cs | Just cs' <- stripPrefix "</SUBTITLE>" cs = Subtitle (reverse s) : lexer' InRule cs'
lexer' (InSubtitle s) (c:cs) = lexer' (InSubtitle (c:s)) cs

-- Proof counters
lexer' InRule cs | Just cs' <- stripPrefix "<COUNTER>" cs = lexer' (InCounter []) cs'
lexer' (InCounter s) cs | Just cs' <- stripPrefix "</COUNTER>" cs = Counter (mkCounter s) : lexer' InRule cs'
  where
    mkCounter :: String -> Int
    mkCounter s
      | all isDigit s && not (null s) = read (reverse s)
      | otherwise = error $ "Counter is not a valid integer"
lexer' (InCounter s) (c:cs)
  | isSpace c = lexer' (InCounter s) cs
  | isDigit c = lexer' (InCounter (c:s)) cs
  | otherwise = error $ "Unexpected character: " ++ [c]

-- References to applied rules
lexer' InRule cs | Just cs' <- stripPrefix "<RULEREF>" cs = RuleRefOpen : lexer' (InRuleRef Nothing) cs'
lexer' (InRuleRef _) cs | Just cs' <- stripPrefix "</RULEREF>" cs = RuleRefClose : lexer' InRule cs'

-- Defn
lexer' (InRuleRef Nothing) cs | Just cs' <- stripPrefix "<DEFN> `" cs = lexer' (InRuleRef (Just (Defn ""))) cs'
lexer' (InRuleRef (Just (Defn s))) cs | Just cs' <- stripPrefix "` </DEFN>" cs = RRDefn (reverse s) : lexer' (InRuleRef Nothing) cs'
lexer' (InRuleRef (Just (Defn s))) (c:cs) = lexer' (InRuleRef (Just (Defn (c:s)))) cs

-- Local
lexer' (InRuleRef Nothing) cs | Just cs' <- stripPrefix "<LOCAL>" cs = lexer' (InRuleRef (Just (Local ""))) cs'
lexer' (InRuleRef (Just (Local s))) cs | Just cs' <- stripPrefix "</LOCAL>" cs = RRLocal (mkLocal s) : lexer' (InRuleRef Nothing) cs'
  where
    mkLocal :: String -> Int
    mkLocal s
      | all isDigit s && not (null s) = read (reverse s)
      | otherwise = error $ "Local is not a valid integer"
lexer' (InRuleRef (Just (Local s))) (c:cs) = lexer' (InRuleRef (Just (Local (c:s)))) cs

-- Cases and induction
lexer' (InRuleRef Nothing) cs | Just cs' <- stripPrefix "<CASES> `" cs = lexer' (InRuleRef (Just (Cases C "" Nothing))) cs'
lexer' (InRuleRef (Just (Cases C s (Just num)))) cs | Just cs' <- stripPrefix "` </CASES>" cs = RRCases ((reverse s), (mkIndex num)) : lexer' (InRuleRef Nothing) cs'
  where
    mkIndex :: String -> Int
    mkIndex s
      | all isDigit s && not (null s) = read (reverse s)
      | otherwise = error $ "Index is not a valid integer"
    
lexer' (InRuleRef Nothing) cs | Just cs' <- stripPrefix "<INDUCTION> `" cs = lexer' (InRuleRef (Just (Cases I "" Nothing))) cs'
lexer' (InRuleRef (Just (Cases I s (Just num)))) cs | Just cs' <- stripPrefix "` </INDUCTION>" cs = RRInduction ((reverse s), (mkIndex num)) : lexer' (InRuleRef Nothing) cs'
  where
    mkIndex :: String -> Int
    mkIndex s
      | all isDigit s && not (null s) = read (reverse s)
      | otherwise = error $ "Index is not a valid integer"

lexer' (InRuleRef (Just (Cases t s Nothing))) cs | Just cs' <- stripPrefix "` `" cs = lexer' (InRuleRef (Just (Cases t s (Just "")))) cs'
lexer' (InRuleRef (Just (Cases t s Nothing))) (c:cs) = lexer' (InRuleRef (Just (Cases t (c:s) Nothing))) cs
lexer' (InRuleRef (Just (Cases t s (Just s')))) (c:cs) = lexer' (InRuleRef (Just (Cases t s (Just (c:s'))))) cs

-- Tag rules
lexer' state cs = case res of
  [(x, cs')] -> x : lexer' state cs'
  _ -> case cs of
    [] -> []
    (_:cs') -> lexer' state cs'
  where
    res = [(x, cs') | (x, Just cs') <- tagToks]

    tagToks :: [(Token, Maybe String)]
    tagToks = map (fmap (flip stripPrefix cs)) [
        (RuleItemOpen, "<RI>"),
        (RuleItemClose, "</RI>"),
        (SubtreesOpen, "<SUBTREES>"),
        (SubtreesClose, "</SUBTREES>"),
        (SubtreeOpen, "<SUBTREE>"),
        (SubtreeClose, "</SUBTREE>"),
        (PremiseOpen, "<PREMISE>"),
        (PremiseClose, "</PREMISE>"),
        (ConclusionOpen, "<CONCLUSION>"),
        (ConclusionClose, "</CONCLUSION>"),
        (ProofOpen, "<PROOF>"),
        (ProofClose, "</PROOF>"),
        (GoalOpen, "<GOAL>"),
        (GoalClose, "</GOAL>"),
        (RRRefl, "<REFL />"),
        (RRTrans, "<TRANS />"),
        (RRInject, "<INJECT />"),
        (RRDistinctOpen, "<DISTINCT>"),
        (RRDistinctClose, "</DISTINCT>"),
        (RRElimOpen, "<ELIM>"),
        (RRElimClose, "</ELIM>"),
        (RRRewriteOpen, "<REWRITE>"),
        (RRRewriteClose, "</REWRITE>"),
        (RRFlipped, "<FLIPPED />"),
        (RRLHS, "<LHS />"),
        (RRRHS, "<RHS />")
      ]

