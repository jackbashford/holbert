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

%token
	heading     			{ Heading $$ }
	paragraph   			{ Paragraph $$ }
    syntaxTok               { SyntaxToken $$ }
	kind					{ Kind $$ }
	name					{ Name $$ }
	style					{ Style $$ }
	subtitle				{ Subtitle $$ }
	vars					{ Vars $$ }
	termString				{ TermString $$ }
	counter					{ Counter $$ }
    defn      				{ RRDefn $$ }
    local      				{ RRLocal $$ }
    cases      				{ RRCases $$ }
    induction      			{ RRInduction $$ }
	"<S>"       			{ SyntaxOpen }
	"</S>"      			{ SyntaxClose }
	"<R>"       			{ RuleOpen }
	"</R>"      			{ RuleClose }
	"<RI>"          		{ RuleItemOpen }
	"</RI>"         		{ RuleItemClose }
	"<SUBTREES>"    		{ SubtreesOpen }
	"</SUBTREES>"   		{ SubtreesClose }
	"<SUBTREE>"     		{ SubtreeOpen }
	"</SUBTREE>"    		{ SubtreeClose }
	"<PREMISE>"     		{ PremiseOpen }
	"</PREMISE>"    		{ PremiseClose }
	"<CONCLUSION>"  		{ ConclusionOpen }
	"</CONCLUSION>" 		{ ConclusionClose }
	"<PROOF>"       		{ ProofOpen }
	"</PROOF>"      		{ ProofClose }
	"<GOAL>"        		{ GoalOpen }
	"</GOAL>"       		{ GoalClose }
	"<RULEREF>"				{ RuleRefOpen }
	"</RULEREF>" 			{ RuleRefClose }
    "<REFL />"	    		{ RRRefl }
    "<TRANS />"	    		{ RRTrans }
    "<INJECT />"			{ RRInject }
    "<DISTINCT>"			{ RRDistinctOpen }
    "</DISTINCT>"			{ RRDistinctClose }
    "<ELIM>"	    		{ RRElimOpen }
    "</ELIM>"	    		{ RRElimClose }
    "<REWRITE>"	    		{ RRRewriteOpen }
    "</REWRITE>"			{ RRRewriteClose }
    "<FLIPPED />"			{ RRFlipped }
    "<LHS />"	    		{ RRLHS }
    "<RHS />"	    		{ RRRHS }

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
	 | Rule					{ I.Rule $1 }

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

Rule :: { R.Rule }
Rule : "<R>" kind RuleItems NamedProps "</R>" { R.R $2 (reverse $3) (reverse $4) }

RuleItems :: { [R.RuleItem] }
RuleItems : {- empty -} { [] }
          | RuleItems RuleItem { $2 : $1 }

RuleItem :: { R.RuleItem }
RuleItem : "<RI>" name Prop	"</RI>"				{ R.RI (MS.ms $2) $3 Nothing }
         | "<RI>" name Prop ProofState "</RI>"	{ R.RI (MS.ms $2) $3 (Just $4) }

Prop :: { P.Prop }
Prop : Vars Conclusion Premises { P.Forall $1 (reverse $3) (T.Unparsed $2) }

ProofState :: { R.ProofState }
ProofState : "<PROOF>" Tree counter "</PROOF>" { R.PS $2 $3 } -- TODO

Tree :: { PT.ProofTree }
Tree : DisplayData "<GOAL>" Prop "</GOAL>" { PT.PT $1 (vars $3) (premises $3) (conclusion $3) Nothing }
     | DisplayData "<GOAL>" Prop "</GOAL>" "<RULEREF>" RuleRef "</RULEREF>" Subtrees { PT.PT $1 (vars $3) (premises $3) (conclusion $3) (Just ($6, $8)) }

Subtrees :: { [PT.ProofTree] }
Subtrees : {- empty -} { [] }
         | "<SUBTREES>" SubtreeSeq "</SUBTREES>" { reverse $2 }

SubtreeSeq :: { [PT.ProofTree] }
SubtreeSeq : SubtreeSeq Subtree { $2 : $1 }
           | {- empty -} 		{ [] }

Subtree :: { PT.ProofTree }
Subtree : "<SUBTREE>" Tree "</SUBTREE>" { $2 }

DisplayData :: { Maybe PT.ProofDisplayData }
DisplayData : style subtitle	{ Just (PT.PDD $1 (MS.ms $2)) }
            | {- empty -} 		{ Nothing }

RuleRef :: { P.RuleRef }
RuleRef : defn 														{ P.Defn (MS.ms $1) }
        | local 													{ P.Local $1 }
		| cases 													{ P.Cases (MS.ms (fst $1)) (snd $1) }
		| induction 												{ P.Induction (MS.ms (fst $1)) (snd $1) }
		| "<REFL />" 												{ P.Refl }
		| "<TRANS />" 												{ P.Transitivity }
		| "<DISTINCT>" RuleRef "</DISTINCT>" 						{ P.Distinctness $2 }
		| "<INJECT />" 												{ P.Injectivity }
		| "<REWRITE>" RuleRef Flipped CalcLocation "</REWRITE>" 	{ P.Rewrite $2 $3 $4 }
		| "<ELIM>" RuleRef RuleRef "</ELIM>" 						{ P.Elim $2 $3 }

Flipped :: { Bool }
Flipped : "<FLIPPED />" 	{ True }
        | {- empty -} 		{ False }

CalcLocation :: { (Maybe P.CalcLocation) }
CalcLocation : "<LHS />" 		{ Just P.LHS }
             | "<RHS />" 		{ Just P.RHS }
			 | {- empty -} 		{ Nothing }

Vars :: { [T.Name] }
Vars : {- empty -} { [] }
     | vars		   { map MS.ms $1 }

Premises :: { [P.Prop] }
Premises : {- empty -} { [] }
		 | Premises Premise { $2 : $1 }

Premise :: { P.Prop }
Premise : "<PREMISE>" Prop "</PREMISE>" { $2 }

Conclusion : "<CONCLUSION>" termString "</CONCLUSION>" { $2 }

NamedProps :: { [P.NamedProp] }
NamedProps : {- empty -} { [] }
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

{-
type RuleState = ([[T.Name]], [SD.SyntaxDecl])

pushVars :: [T.Name] -> State RuleState ()
pushVars v = State $ \(vs, tbl) -> ((), (v:vs, tbl))

popVars :: State RuleState ()
popVars = State $ \(vs, tbl) -> case vs of
  [] -> ((), [])
  (v:vs') -> ((), vs')

-- Should never need to pop decls, we can't un-declare syntax :)
pushDecl :: SD.SyntaxDecl -> State RuleState ()
pushDecl decl = State $ \(vs, tbl) -> ((), (vs, decl : tbl))

getCts :: State RuleState RuleState
getCts = State $ \(vs, tbl) -> ((vs, tbl), (vs, tbl))
-}
parseError :: [Token] -> a
parseError tks = error $ "Parse error! Tks: " ++ show tks

-- parseDoc needs to use the Earley parser to parse the mixfix operators
-- Its type is `Maybe Document` in case we want to cause a failure at any point here - the actual parsing into a Document by `parse` should never fail, failure should just produce an empty list.
parseDoc :: String -> Maybe Document
parseDoc inp = let ts = lexer inp in (trace $ "toks: " ++ show (unlines (map show ts))) $ Just $ parseMixfix $ parse $ lexer inp

parseMixfix :: Document -> Document
parseMixfix = go []
  where
    go _ [] = []
    go sds (i:is) = case i of
      I.Paragraph _ -> i : (go sds is)
      I.Heading _ -> i : (go sds is)
      I.SyntaxDecl (SD.SyntaxDecl s) -> i : (go (sds ++ s) is)
      I.Rule r -> I.Rule (parseRule r sds) : (go sds is)

parseRule :: R.Rule -> SR.SyntaxTable -> R.Rule
parseRule (R.R t is ps) sds = R.R t (map (parseRuleItem sds) is) (map (\p -> fmap (parseProp sds []) p) ps)

parseRuleItem :: SR.SyntaxTable -> R.RuleItem -> R.RuleItem
parseRuleItem sds (R.RI name prop st) = R.RI name (parseProp sds [] prop) ((parseProofState sds) <$> st)

parseProp :: SR.SyntaxTable -> [T.Name] -> P.Prop -> P.Prop
parseProp sds vsUpper (P.Forall vs ps (T.Unparsed conc)) = P.Forall vs (map (parseProp sds (vs ++ vsUpper)) ps) (parseTerm sds (vs ++ vsUpper) conc)

parseTerm :: SR.SyntaxTable -> [T.Name] -> String -> T.Term
parseTerm sds vs str = case (SR.parse sds vs (MS.ms str)) of
  Left s -> error  $ "Failure to parse mixfix operator: " ++ show s
  Right t -> t

parseProofState :: SR.SyntaxTable -> R.ProofState -> R.ProofState
parseProofState sds (R.PS tree counter) = R.PS (parseProofTree sds [] tree) counter

parseProofTree :: SR.SyntaxTable -> [T.Name] -> PT.ProofTree -> PT.ProofTree
parseProofTree sds vsUpper (PT.PT display vs ps (T.Unparsed term) subs) = PT.PT display vs (map (parseProp sds (vs ++ vsUpper)) ps) (parseTerm sds (vs ++ vsUpper) term) (parsedSubtrees)
  where
	parsedSubtrees = case subs of
	  Nothing -> Nothing
	  Just (rr, subs) -> Just (rr, map (parseProofTree sds (vs ++ vsUpper)) subs)
}
