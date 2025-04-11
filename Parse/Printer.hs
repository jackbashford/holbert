{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Parse.Printer where

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
import Debug.Trace(trace)
import Data.JSString(JSString, pack)

print :: E.Document -> IO JSString
print doc = (return . MS.fromMisoString) $ MS.intercalate "\n\n" (printHelper doc [])

printHelper :: E.Document -> SR.SyntaxTable -> [MS.MisoString]
printHelper [x] syntax = [printItem syntaxTable x]
  where
    syntaxTable :: SR.SyntaxTable
    syntaxTable = case x of
      (I.SyntaxDecl (SD.SyntaxDecl items)) -> items ++ syntax
      _ -> syntax
printHelper (x:xs) syntax = printItem syntaxTable x : printHelper xs syntaxTable
  where
    syntaxTable :: SR.SyntaxTable
    syntaxTable = case x of
      (I.SyntaxDecl (SD.SyntaxDecl items)) -> items ++ syntax
      _ -> syntax

printItem :: SR.SyntaxTable -> I.Item -> MS.MisoString
printItem syntaxTable = \case
  (I.Heading (H.Heading level body)) -> "<H" <> MS.ms level <> ">" <> body <> "</H" <> MS.ms level <> ">"
  (I.Paragraph (PG.Paragraph body)) -> "<P>" <> MS.ms body <> "</P>"
  (I.SyntaxDecl (SD.SyntaxDecl items)) -> "<S>\n" <> showSyntax items <> "\n</S>"
  (I.Rule cts) -> "<R>" <> showRule syntaxTable cts <> "</R>"
  -- (I.Rule cts) -> "<R>" <> MS.ms (show cts) <> "</R>"

showSyntax :: SR.SyntaxTable -> MS.MisoString
showSyntax = (MS.intercalate "\n") . map showDecl
  where
    showDecl :: (Int, MS.MisoString, EPM.Associativity) -> MS.MisoString
    showDecl (p, s, a) = (MS.ms p) <> "\n" <> s <> "\n" <> assoc'
      where
        assoc' :: MS.MisoString
        assoc' = case a of
          LeftAssoc -> "left"
          RightAssoc -> "right"
          NonAssoc -> "no"

showRule :: SR.SyntaxTable -> R.Rule -> MS.MisoString
showRule syntaxTable (R.R ruleKind items props) = MS.ms (show ruleKind) <> "\n" <> (MS.intercalate "\n" (map (showItem syntaxTable) items)) <> ""

showItem :: SR.SyntaxTable -> R.RuleItem -> MS.MisoString
showItem syntaxTable (R.RI ruleName prop proofState) = "<RI>\n" <> ruleName <> "\n" <> showProp Nothing syntaxTable prop <> "\n" <> showPS proofState <> "\n</RI>"

-- This needs prettyprinting
-- We can do this by keeping a context of both the 'parent' prop and the 'current' prop, and updating a list (the P.Path) so we can use getConclusionString.
showProp :: Maybe (P.Path, P.Prop) -> SR.SyntaxTable -> P.Prop -> MS.MisoString
showProp Nothing syntaxTable p@(P.Forall vars assumptions result) = (MS.intercalate "," ((\x -> if null x then ["{}"] else x) (map MS.ms vars))) <> "\n" <> (MS.intercalate "\n" (map (\(i, x) -> ". " <> showProp (Just ([i], p)) syntaxTable x) (zip [0..] assumptions))) <> "\n|-" <> (trace (show p) $ P.getConclusionString syntaxTable [] p) -- showTerm syntaxTable result

showProp (Just (path, parent)) syntaxTable prop@(P.Forall vars assumptions result) = (MS.intercalate "," ((\x -> if null x then ["{}"] else x) (map MS.ms vars))) <> "\n" <> (MS.intercalate "\n" (map (\(i, x) -> ". " <> showProp (Just (i : path, parent)) syntaxTable x) (zip [0..] assumptions))) <> "\n|-" <> (trace (show (path, parent, prop)) $ P.getConclusionString syntaxTable path parent)

showPS :: Maybe R.ProofState -> MS.MisoString
showPS (Just ps) = MS.ms $ show ps
showPS (Nothing) = "Empty proof state"

showTerm :: SR.SyntaxTable -> T.Term -> MS.MisoString
showTerm syntaxTable = MS.ms . show
