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
import Data.JSString(JSString, pack)

print :: E.Document -> IO JSString
print doc = return $ MS.intercalate "\n\n" (map printItem doc)

printItem :: I.Item -> MS.MisoString
printItem = \case
  (I.Heading (H.Heading level body)) -> "<H" <> MS.ms level <> ">" <> body <> "</H" <> MS.ms level <> ">"
  (I.Paragraph (PG.Paragraph body)) -> "<P>" <> MS.ms body <> "</P>"
  (I.SyntaxDecl (SD.SyntaxDecl items)) -> "<S>\n" <> showSyntax items <> "\n</S>"
  (I.Rule {}) -> "<R></R>"

showSyntax :: [(Int, MS.MisoString, EPM.Associativity)] -> MS.MisoString
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
