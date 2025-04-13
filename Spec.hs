{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where
import Miso
import Miso.String(MisoString)
import Editor (runAction, EditorAction (..), Editor (..), initialEditor)
import qualified ImportExport
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
import qualified Parse.Printer as Printer
import qualified Parse.Parser as Parser
import Debug.Trace(trace)
import View.Editor (viewEditor)

foreign import javascript unsafe "$r = document.location.search.slice(1);"
  urlparameter :: IO MisoString

main :: IO ()
main = go tests 0
  where
    go [] _ = putStrLn "Success"
    go (t:ts) n = case t of
      (_, True) -> go ts (n + 1)
      (tc, False) -> putStrLn $ "Test failure: " ++ tc

tests :: [(String, Bool)]
tests = [
  ("Sanity check", 1 + 1 == 2),
  ("Simple paragraph", t [I.Paragraph (PG.Paragraph "Simple paragraph test")]),
  ("Simple heading", t [I.Heading (H.Heading 3 "Simple heading at level 3"), I.Heading (H.Heading 1 "Simple heading at level 1")])
  ]
  where
    t = constructParserTest

constructParserTest :: E.Document -> Bool
constructParserTest doc = case Parser.parseDoc (MS.fromMisoString (Printer.printDoc doc)) of
  Nothing -> False
  Just d' -> d' == doc
