{
{-# LANGUAGE OverloadedStrings #-}
module Parse.Parser where

import Debug.Trace(trace)
import Data.Char(isDigit, isSpace)
import Data.List(stripPrefix, span, intercalate)
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
import Data.JSString(JSString)
import Parse.Lexer
import Editor(Document)
import Control.Monad.State
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { State (SR.SyntaxTable, [[T.Name]]) } { (>>=) } { pure }

%token
    heading         { Heading $$ }
    paragraph       { Paragraph $$ }
    syntaxTok       { SyntaxToken $$ }
    kind            { Kind $$ }
    name            { Name $$ }
    style           { Style $$ }
    subtitle        { Subtitle $$ }
    vars            { Vars $$ }
    termString      { TermString $$ }
    counter         { Counter $$ }
    defn            { RRDefn $$ }
    local           { RRLocal $$ }
    cases           { RRCases $$ }
    induction       { RRInduction $$ }
    "<S>"           { SyntaxOpen }
    "</S>"          { SyntaxClose }
    "<R>"           { RuleOpen }
    "</R>"          { RuleClose }
    "<RI>"          { RuleItemOpen }
    "</RI>"         { RuleItemClose }
    "<SUBTREES>"    { SubtreesOpen }
    "</SUBTREES>"   { SubtreesClose }
    "<SUBTREE>"     { SubtreeOpen }
    "</SUBTREE>"    { SubtreeClose }
    "<PREMISE>"     { PremiseOpen }
    "</PREMISE>"    { PremiseClose }
    "<CONCLUSION>"  { ConclusionOpen }
    "</CONCLUSION>" { ConclusionClose }
    "<PROOF>"       { ProofOpen }
    "</PROOF>"      { ProofClose }
    "<DISPLAY>"     { DisplayOpen }
    "</DISPLAY>"    { DisplayClose }
    "<GOAL>"        { GoalOpen }
    "</GOAL>"       { GoalClose }
    "<RULEREF>"     { RuleRefOpen }
    "</RULEREF>"    { RuleRefClose }
    "<REFL />"      { RRRefl }
    "<TRANS />"     { RRTrans }
    "<INJECT />"    { RRInject }
    "<DISTINCT>"    { RRDistinctOpen }
    "</DISTINCT>"   { RRDistinctClose }
    "<ELIM>"        { RRElimOpen }
    "</ELIM>"       { RRElimClose }
    "<REWRITE>"     { RRRewriteOpen }
    "</REWRITE>"    { RRRewriteClose }
    "<FLIPPED />"   { RRFlipped }
    "<LHS />"       { RRLHS }
    "<RHS />"       { RRRHS }

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
     | Rule                 { I.Rule $1 }

Heading :: { H.Heading }
Heading : heading { H.Heading (fst $1) (MS.ms (snd $1)) }

Paragraph :: { PG.Paragraph }
Paragraph : paragraph { PG.Paragraph (MS.ms $1) }

SyntaxDecl :: { SD.SyntaxDecl }
SyntaxDecl : "<S>" SyntaxItems "</S>" {% insertDecls $2 }

SyntaxItems :: { [Maybe (Int, MS.MisoString, EPM.Associativity)] }
SyntaxItems : SyntaxItem               { [$1] }
            | SyntaxItems SyntaxItem   { $2 : $1 }

SyntaxItem :: { Maybe (Int, MS.MisoString, EPM.Associativity) }
SyntaxItem : syntaxTok syntaxTok syntaxTok { constructSyntaxItem $1 $2 $3 }

Rule :: { R.Rule }
Rule : "<R>" kind RuleItems NamedProps "</R>" { R.R $2 (reverse $3) (reverse $4) }

RuleItems :: { [R.RuleItem] }
RuleItems : {- empty -}        { [] }
          | RuleItems RuleItem { $2 : $1 }

RuleItem :: { R.RuleItem }
RuleItem : "<RI>" name Prop "</RI>"               { R.RI (MS.ms $2) $3 Nothing }
         | "<RI>" name Prop ProofState "</RI>"    { R.RI (MS.ms $2) $3 (Just $4) }

Prop :: { P.Prop }
Prop : Vars Conclusion Premises {% buildProp $1 $2 $3 }

ProofProp :: { P.Prop }
ProofProp : Vars Conclusion Premises {% buildProofProp $1 $2 $3 }

ProofState :: { R.ProofState }
ProofState : "<PROOF>" Tree counter "</PROOF>" { R.PS $2 $3 }

Tree :: { PT.ProofTree }
Tree : DisplayData "<GOAL>" ProofProp "</GOAL>" OptionalSubtrees {% removeVars (PT.PT $1 (vars $3) (premises $3) (conclusion $3) $5) }

OptionalSubtrees :: { Maybe (P.RuleRef, [PT.ProofTree]) }
OptionalSubtrees : {- empty -}                               { Nothing }
                 | "<RULEREF>" RuleRef "</RULEREF>" Subtrees { Just ($2, $4) }

Subtrees :: { [PT.ProofTree] }
Subtrees : {- empty -} { [] }
         | "<SUBTREES>" SubtreeSeq "</SUBTREES>" { reverse $2 }

SubtreeSeq :: { [PT.ProofTree] }
SubtreeSeq : {- empty -}        { [] }
           | SubtreeSeq Subtree { $2 : $1 }

Subtree :: { PT.ProofTree }
Subtree : "<SUBTREE>" Tree "</SUBTREE>" { $2 }

DisplayData :: { Maybe PT.ProofDisplayData }
DisplayData : {- empty -}                             { Nothing }
            | "<DISPLAY>" style subtitle "</DISPLAY>" { Just (PT.PDD $2 (MS.ms $3)) }

RuleRef :: { P.RuleRef }
RuleRef : defn                                                      { P.Defn (MS.ms $1) }
        | local                                                     { P.Local $1 }
        | cases                                                     { P.Cases (MS.ms (fst $1)) (snd $1) }
        | induction                                                 { P.Induction (MS.ms (fst $1)) (snd $1) }
        | "<REFL />"                                                { P.Refl }
        | "<TRANS />"                                               { P.Transitivity }
        | "<DISTINCT>" RuleRef "</DISTINCT>"                        { P.Distinctness $2 }
        | "<INJECT />"                                              { P.Injectivity }
        | "<REWRITE>" RuleRef Flipped CalcLocation "</REWRITE>"     { P.Rewrite $2 $3 $4 }
        | "<ELIM>" RuleRef RuleRef "</ELIM>"                        { P.Elim $2 $3 }

Flipped :: { Bool }
Flipped : {- empty -}       { False }
        | "<FLIPPED />"     { True }

CalcLocation :: { (Maybe P.CalcLocation) }
CalcLocation : {- empty -}   { Nothing }
             | "<LHS />"     { Just P.LHS }
             | "<RHS />"     { Just P.RHS }

Vars :: { [T.Name] }
Vars : {- empty -} {% insertVars [] }
     | vars        {% insertVars (map MS.ms $1) }

Premises :: { [P.Prop] }
Premises : {- empty -}      { [] }
         | Premises Premise { $2 : $1 }

Premise :: { P.Prop }
Premise : "<PREMISE>" Prop "</PREMISE>" { $2 }

Conclusion :: { String }
Conclusion : "<CONCLUSION>" termString "</CONCLUSION>" { $2 }

NamedProps :: { [P.NamedProp] }
NamedProps : {- empty -}          { [] }
           | NamedProps NamedProp { $2 : $1 }

NamedProp :: { P.NamedProp }
NamedProp : "<RULEREF>" RuleRef "</RULEREF>" Prop { ($2, $4) }

{
vars :: P.Prop -> [T.Name]
vars (P.Forall vs _ _) = vs

premises :: P.Prop -> [P.Prop]
premises (P.Forall _ ps _) = ps

conclusion :: P.Prop -> T.Term
conclusion (P.Forall _ _ c) = c

type ContextState = State (SR.SyntaxTable, [[T.Name]])

insertDecls :: [Maybe (Int, MS.MisoString, EPM.Associativity)] -> ContextState SD.SyntaxDecl
insertDecls decls = modify (\(s, v) -> (s ++ toInsert, v)) *> (return (SD.SyntaxDecl toInsert)) 
  where
    toInsert = reverse (catMaybes decls)

insertVars :: [T.Name] -> ContextState [T.Name]
insertVars vs = modify (\(s, v) -> (s, (reverse vs) : v)) *> (return vs)

removeVars :: a -> ContextState a
removeVars val = modify (fmap tail) *> (return val)

buildProp :: [T.Name] -> String -> [P.Prop] -> ContextState P.Prop
buildProp = buildProp' True

buildProofProp :: [T.Name] -> String -> [P.Prop] -> ContextState P.Prop
buildProofProp = buildProp' False

-- isRealProp is False when this is a 'Prop' we have constructed as part of our ProofTree.
buildProp' :: Bool -> [T.Name] -> String -> [P.Prop] -> ContextState P.Prop
buildProp' isRealProp vars result premises = modify act >> mkProp <$> get
  where
    act :: (SR.SyntaxTable, [[T.Name]]) -> (SR.SyntaxTable, [[T.Name]])
    act s | not isRealProp = s
    act (s, (v:vs)) | (reverse v) == vars = (s, vs)
    act _ = error "Non-matching variable scopes found!"

    mkProp :: (SR.SyntaxTable, [[T.Name]]) -> P.Prop
    mkProp (s, vs) = P.Forall vars (reverse premises) (parseTerm s (correctedVars vs) result)

    correctedVars :: [[T.Name]] -> [T.Name]
    correctedVars v
      | isRealProp = (reverse vars) ++ concat v
      | otherwise  = concat v

parseError :: [Token] -> a
parseError tks = error $ "Parse error! Tks: " ++ show tks

parseDoc :: String -> Maybe Document
parseDoc inp = let res = evalState (parse (lexer inp)) ([], []) in if null res then Nothing else Just res

-- Use the Earley parser to parse the terms
parseTerm :: SR.SyntaxTable -> [T.Name] -> String -> T.Term
parseTerm sds vs str = case (SR.parse sds vs (MS.ms str)) of
  Left s -> error  $ "Failure to parse mixfix operator: " ++ show s
  Right t -> t
}
