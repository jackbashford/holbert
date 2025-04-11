{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where
import Miso
import Miso.String(MisoString)
import Editor (runAction, EditorAction (..), Editor (..), initialEditor)
import qualified ImportExport
import View.Editor (viewEditor)

foreign import javascript unsafe "$r = document.location.search.slice(1);"
  urlparameter :: IO MisoString

main :: IO ()
main = go tests 0
  where
    go [] _ = putStrLn "Success"
    go (t:ts) n = case t of
      True -> go ts (n + 1)
      False -> putStrLn $ "Test failure at index: " ++ show n

tests :: [Bool]
tests = [
  1 + 1 == 2
  ]
