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

printDoc :: E.Document -> JSString
printDoc doc = MS.fromMisoString $ MS.intercalate "\n\n" (printHelper doc [])

printHelper :: E.Document -> SR.SyntaxTable -> [MS.MisoString]
printHelper [] _ = []
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
showRule syntaxTable (R.R ruleKind items props) = MS.ms (show ruleKind) <> "\n" <> MS.intercalate "\n" (map (showItem syntaxTable) items) <> "\n<PROPS>\n" <> MS.intercalate "\n" (map (showNamedProp syntaxTable) props) <> "\n</PROPS>"

showItem :: SR.SyntaxTable -> R.RuleItem -> MS.MisoString
showItem syntaxTable (R.RI ruleName prop proofState) = "<RI>\n" <> ruleName <> "\n" <> showProp' syntaxTable prop <> "\n" <> showPS syntaxTable proofState <> "\n</RI>"

showNamedProp :: SR.SyntaxTable -> P.NamedProp -> MS.MisoString
showNamedProp syntaxTable (ruleRef, p) = "Rule: " <> MS.ms (show ruleRef) <> "\n" <> (showProp' syntaxTable p)

showProp' :: SR.SyntaxTable -> P.Prop -> MS.MisoString
showProp' tbl prop = showProp [] prop tbl prop

-- This needs prettyprinting
-- We can do this by keeping a context of both the 'parent' prop and the 'current' prop, and updating a list (the P.Path) so we can use getConclusionString.
showProp :: P.Path -> P.Prop -> SR.SyntaxTable -> P.Prop -> MS.MisoString
showProp path parent syntaxTable (P.Forall vars premises result) = (MS.intercalate "," ((\x -> if null x then ["{}"] else x) (map MS.ms vars))) <> "\n" <> MS.intercalate "\n" (map (\(i, x) -> MS.unlines $ map (\s -> ". " <> s) (MS.lines $ showProp (i : path) parent syntaxTable x)) (zip [0..] premises)) <> "\n|-" <> P.getConclusionString syntaxTable path parent

-- showProp (Just (path, parent)) syntaxTable p@(P.Forall vars assumptions result) = (MS.intercalate "," ((\x -> if null x then ["{}"] else x) (map MS.ms vars))) <> "\n" <> (MS.intercalate "\n" (map (\(i, x) -> ". " <> showProp (Just (i : path, parent)) syntaxTable x) (zip [0..] assumptions))) <> "\n|-" <> P.getConclusionString syntaxTable path parent

showPS :: SR.SyntaxTable -> Maybe R.ProofState -> MS.MisoString
showPS _ (Nothing) = "Empty proof state"
showPS tbl (Just (R.PS tree counter)) = "<PROOF>\n" <> MS.ms (showTree tbl tree) <> "\n" <> MS.ms (show counter) <> "\n</PROOF>"

showTree :: SR.SyntaxTable -> PT.ProofTree -> MS.MisoString
showTree tbl pt@(PT.PT displayData vars premises result subtree) = "<DISPLAY>\n" <> MS.ms (show displayData) <> "\n</DISPLAY>\n<PROVING>\n" <> (trace ("Base tree: " ++ show pt) $ showProp' tbl parentProp) <> "\n</PROVING>" <> prettySubtree
  where
    parentProp :: P.Prop
    parentProp = (P.Forall vars premises result)

    fauxPremiseParent :: PT.ProofTree -> P.Prop
    fauxPremiseParent (PT.PT _ goalVars goalPremises goalResult _) = P.Forall vars ((P.Forall goalVars goalPremises goalResult) : premises) result

    prettySubtree :: MS.MisoString
    -- prettySubtree = "<SUBTREE TODO>"
    prettySubtree = case subtree of
      Nothing -> ""
      Just (ruleRef, subtrees) -> "\n<RULEREF>" <> MS.ms (show ruleRef) <> "</RULEREF>\n<SUBTREES>\n" <> MS.unlines (map (\x -> ". " <> x) $ MS.lines $ MS.intercalate "\n" (map (\st -> showSubtree [0] tbl (fauxPremiseParent st) st) subtrees)) <> "</SUBTREES>"

showSubtree :: P.Path -> SR.SyntaxTable -> P.Prop -> PT.ProofTree -> MS.MisoString
showSubtree path tbl fauxParent@(P.Forall pVars pPremises pResult) pt@(PT.PT displayData vars premises result subtree) = "<DISPLAY>\n" <> MS.ms (show displayData) <> "\n</DISPLAY>\n<PROVING>\n" <> (trace ("Subtree: " ++ show pt ++ "\nparent:\n" ++ show fauxParent ++ "\npath:" ++ show path) $ showProp path fauxParent tbl goalProp) <> "\n</PROVING>" <> prettySubtree
  where
    goalProp :: P.Prop
    goalProp = (P.Forall vars premises result)
    -- TODO this allows for proper printing, but is not correct because it changes the variables of the proofs
    -- Can we change this by using a path like [0] and manually inserting the subgoal as a premise?

    fauxPremiseParent :: P.Path -> PT.ProofTree -> P.Prop -> P.Prop
    fauxPremiseParent [] (PT.PT _ goalVars goalPremises goalResult _) (P.Forall currPVars currPPremises currPResult) = P.Forall currPVars ((P.Forall goalVars goalPremises goalResult) : currPPremises) currPResult
    fauxPremiseParent (i:ps) pt (P.Forall currPVars (toReplace : currPPremises) currPResult) = P.Forall currPVars ((fauxPremiseParent ps pt toReplace) : currPPremises) currPResult

    prettySubtree :: MS.MisoString
    -- prettySubtree = "<SUBTREE TODO>"
    prettySubtree = case subtree of
      Nothing -> ""
      Just (ruleRef, subtrees) -> "\n<RULEREF>" <> MS.ms (show ruleRef) <> "\n</RULEREF>\n<SUBTREES>\n" <> MS.unlines (map (\x -> ". " <> x) $ MS.lines $ MS.intercalate "\n" (map (\(i, st) -> showSubtree (0 : path) tbl (fauxPremiseParent path st fauxParent) st) (zip [0..] subtrees))) <> "</SUBTREES>"

