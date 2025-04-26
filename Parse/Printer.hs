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
import Data.JSString(JSString)

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
  (I.Rule cts) -> "<R>\n" <> showRule syntaxTable cts <> "\n</R>"

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
showRule syntaxTable (R.R ruleKind items props) = "<KIND>" <> MS.ms (show ruleKind) <> "</KIND>\n" <> MS.intercalate "\n\n" (map (showItem syntaxTable) items) <> namedProps
  where
    namedProps :: MS.MisoString
    namedProps
      | null props = ""
      | otherwise = "\n<PROPS>\n" <> MS.intercalate "\n" (map (showNamedProp syntaxTable) props) <> "</PROPS>"

showItem :: SR.SyntaxTable -> R.RuleItem -> MS.MisoString
showItem syntaxTable (R.RI ruleName prop proofState) = "<RI>\n<NAME>" <> ruleName <> "</NAME>\n" <> showProp' syntaxTable prop <> showPS syntaxTable proofState <> "</RI>"

showNamedProp :: SR.SyntaxTable -> P.NamedProp -> MS.MisoString
showNamedProp syntaxTable (ruleRef, p) = "<RULEREF>" <> showRuleRef ruleRef <> "</RULEREF>\n" <> (showProp' syntaxTable p)

showProp' :: SR.SyntaxTable -> P.Prop -> MS.MisoString
showProp' tbl prop = (showProp [] prop tbl prop) <> "\n"

showProp :: P.Path -> P.Prop -> SR.SyntaxTable -> P.Prop -> MS.MisoString
showProp path parent tbl (P.Forall vars premises _) = printedVars <> printedResult <> printedPremises
  where
    printedVars :: MS.MisoString
    printedVars
      | null vars = ""
      | otherwise = "<VARS>\n" <> MS.intercalate "\n" (map (("  " <>) . MS.ms) vars) <> "\n</VARS>\n"

    printedPremises :: MS.MisoString
    printedPremises
      | null premises = ""
      | otherwise =
        ("\n" <>) $ MS.intercalate "\n" $ map (("\n<PREMISE>\n" <>) . (<> "</PREMISE>") . MS.unlines . map ("    " <>) . MS.lines) $ zipWith printPremise [0..] premises
        where
          printPremise :: Int -> P.Prop -> MS.MisoString
          printPremise i = showProp (i : path) parent tbl

    printedResult :: MS.MisoString
    printedResult = "<CONCLUSION>``" <> P.getConclusionString tbl path parent <> "``</CONCLUSION>"

showPS :: SR.SyntaxTable -> Maybe R.ProofState -> MS.MisoString
showPS _ (Nothing) = ""
showPS tbl (Just (R.PS tree@(PT.PT _ vars premises result _) counter)) = "<PROOF>\n" <> MS.ms (showSubtree [] tbl (P.Forall vars premises result) tree) <> "<COUNTER>" <> MS.ms (show counter) <> "</COUNTER>" <> "\n</PROOF>\n"

showSubtree :: P.Path -> SR.SyntaxTable -> P.Prop -> PT.ProofTree -> MS.MisoString
showSubtree path tbl fauxParent (PT.PT displayData vars premises result subtree) = showDisplayData displayData <> "<GOAL>\n" <> MS.unlines (map ("  " <>) $ MS.lines $ showProp path fauxParent tbl goalProp) <> "</GOAL>\n" <> prettySubtree <> "\n"
  where
    goalProp :: P.Prop
    goalProp = (P.Forall vars premises result)

    -- Allows for pretty-printing by pretending that the subgoal is actually a premise, because printing premises is already done for us.
    fauxPremiseParent :: P.Path -> PT.ProofTree -> P.Prop -> P.Prop
    fauxPremiseParent [] (PT.PT _ goalVars goalPremises goalResult _) (P.Forall currPVars currPPremises currPResult) = P.Forall currPVars ((P.Forall goalVars goalPremises goalResult) : currPPremises) currPResult
    fauxPremiseParent (0:ps) pt (P.Forall currPVars (toReplace : currPPremises) currPResult) = P.Forall currPVars ((fauxPremiseParent ps pt toReplace) : currPPremises) currPResult
    fauxPremiseParent _ _ _ = error "Premise injection failed. :("

    prettySubtree :: MS.MisoString
    prettySubtree = case subtree of
      Nothing -> ""
      Just (ruleRef, subtrees) -> "<RULEREF>" <> showRuleRef ruleRef <> "</RULEREF>"<> (if null subtrees then "" else "\n<SUBTREES>\n" <> MS.unlines (map ("  " <>) $ MS.lines $ MS.intercalate "\n" (map (\st -> "<SUBTREE>\n" <> showSubtree (0 : path) tbl (fauxPremiseParent path st fauxParent) st <> "\n</SUBTREE>\n") subtrees)) <> "</SUBTREES>")

showDisplayData :: Maybe PT.ProofDisplayData -> MS.MisoString
showDisplayData Nothing = ""
showDisplayData (Just (PT.PDD style subtitle)) = "<DISPLAY>\n  <STYLE>" <> MS.ms (show style) <> "</STYLE>\n  <SUBTITLE>" <> subtitle <> "</SUBTITLE>\n"

showRuleRef :: P.RuleRef -> MS.MisoString
showRuleRef rr = case rr of
  P.Defn name -> "<DEFN> `" <> name <> "` </DEFN>"
  P.Local n -> "<LOCAL>" <> MS.ms (show n) <> "</LOCAL>"
  P.Cases name num -> "<CASES> `" <> name <> "` `" <> MS.ms (show num) <> "` </CASES>"
  P.Induction name num -> "<INDUCTION> `" <> name <> "` `" <> MS.ms (show num) <> "` </INDUCTION>"
  P.Refl -> "<REFL />"
  P.Transitivity -> "<TRANS />"
  P.Injectivity -> "<INJECT />"
  P.Distinctness ref -> "<DISTINCT>" <> showRuleRef ref <> "</DISTINCT>"
  P.Elim ref1 ref2 -> "<ELIM>" <> showRuleRef ref1 <> showRuleRef ref2 <> "</ELIM>"
  P.Rewrite ref flipped calcLoc -> "<REWRITE>" <> showRuleRef ref <> (if flipped then "<FLIPPED />" else "") <> calcLoc' <> "</REWRITE>"
    where
      calcLoc' = case calcLoc of
        Nothing -> ""
        Just P.LHS -> "<LHS />"
        Just P.RHS -> "<RHS />"
