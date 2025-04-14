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
  (I.Rule cts) -> "<R>" <> showRule syntaxTable cts <> "\n</R>"
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
showRule syntaxTable (R.R ruleKind items props) = MS.ms (show ruleKind) <> "\n" <> MS.intercalate "\n\n" (map (showItem syntaxTable) items) <> namedProps
  where
    namedProps :: MS.MisoString
    namedProps
      | null props = ""
      | otherwise = "\n<PROPS>\n" <> MS.intercalate "\n" (map (showNamedProp syntaxTable) props) <> "</PROPS>"

showItem :: SR.SyntaxTable -> R.RuleItem -> MS.MisoString
showItem syntaxTable (R.RI ruleName prop proofState) = let leader = "\"" <> ruleName  <> "\" : " in "<RI>\n" <> leader <> (let printedPropLines = MS.lines (showProp' syntaxTable prop) in MS.unlines (init printedPropLines ++ [last printedPropLines <> (MS.replicate (MS.length leader) "-")])) <> showPS syntaxTable proofState <> "</RI>"

showNamedProp :: SR.SyntaxTable -> P.NamedProp -> MS.MisoString
showNamedProp syntaxTable (ruleRef, p) = "Rule: " <> MS.ms (show ruleRef) <> "\n" <> (showProp' syntaxTable p)

showProp' :: SR.SyntaxTable -> P.Prop -> MS.MisoString
showProp' tbl prop = (showProp [] prop tbl prop) <> "\n"

-- This needs prettyprinting
-- We can do this by keeping a context of both the 'parent' prop and the 'current' prop, and updating a list (the P.Path) so we can use getConclusionString.
showProp :: P.Path -> P.Prop -> SR.SyntaxTable -> P.Prop -> MS.MisoString
showProp path parent tbl (P.Forall vars premises result) = let propSt = printedVars <> "|- " <> printedResult in propSt <> "\n" <> printedPremises <> (MS.replicate (MS.length propSt) "-")
  where
    printedVars :: MS.MisoString
    printedVars
      | null vars = ""
      | otherwise = "Forall " <> MS.intercalate ", " (map MS.ms vars) <> " "

    printedPremises :: MS.MisoString
    printedPremises
      | null premises = ""
      | otherwise =
        let premiseLines = map (MS.unlines . map ("|   " <>) . MS.lines) $ zipWith printPremise [0..] premises
            -- premisesTrailer = if length premises == 1 then "" else MS.replicate 40 "-"
        in MS.concat premiseLines -- <> premisesTrailer -- MS.replicate (MS.length (last premiseLines)) "-" <> "\n"
      where
        printPremise :: Int -> P.Prop -> MS.MisoString
        printPremise i = showProp (i : path) parent tbl

    printedResult :: MS.MisoString
    printedResult = "``" <> P.getConclusionString tbl path parent <> "``"

showPS :: SR.SyntaxTable -> Maybe R.ProofState -> MS.MisoString
showPS _ (Nothing) = ""
showPS tbl (Just (R.PS tree@(PT.PT _ vars premises result _) counter)) = "<PROOF>\n" <> MS.ms (showSubtree [] tbl (P.Forall vars premises result) tree) <> "\n" <> MS.ms (show counter) <> "\n</PROOF>"

showSubtree :: P.Path -> SR.SyntaxTable -> P.Prop -> PT.ProofTree -> MS.MisoString
showSubtree path tbl fauxParent@(P.Forall pVars pPremises pResult) pt@(PT.PT displayData vars premises result subtree) = "<DISPLAY>\n" <> MS.ms (show displayData) <> "\n</DISPLAY>\n<PROVING>\n" <> showProp path fauxParent tbl goalProp <> "\n</PROVING>" <> prettySubtree <> "\n"
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
      Just (ruleRef, subtrees) -> "\n<RULEREF>" <> MS.ms (show ruleRef) <> "\n</RULEREF>\n<SUBTREES>\n" <> MS.unlines (map (\x -> ". " <> x) $ MS.lines $ MS.intercalate "\n" (map (\st -> showSubtree (0 : path) tbl (fauxPremiseParent path st fauxParent) st) subtrees)) <> "</SUBTREES>"

