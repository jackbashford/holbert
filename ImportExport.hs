{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ImportExport where
-- Vandelay Industries
import Miso
import Data.JSString
import JavaScript.Web.XMLHttpRequest
import Data.Aeson
import GHCJS.Marshal
import qualified Control.Exception as Exc
import qualified Parse.Parser as Parser
import qualified Parse.Printer as Print
import Editor(Document)

cleanup :: IO a -> IO (Maybe a)
cleanup x = Exc.catch (Just <$> x) handler
  where
    handler exc = return Nothing  `const`  (exc :: Exc.ErrorCall)

import_ :: JSString -> IO (Either JSString Document)
import_ url = do
  response <- xhr $ Request GET url Nothing [] False NoData
  case status response of
    200 ->
      case contents response of
        Nothing -> pure $ Left "empty response"
        Just (s :: JSString) -> do
          s' <- cleanup . parse =<< toJSVal s
          pure $ case s' of
            Nothing -> Left $ "cannot parse imported file" <> s
            Just r  -> Right r
    _ -> pure $ Left "Unsuccessful status code"

export :: JSString -> Document -> IO ()
export fn m = pure (Print.printDoc m) >>= saveAs fn

openFile :: IO (Either JSString Document)
openFile = do
  str <- fileOpenHelper
  let s' = Parser.parseDoc $ unpack (str :: JSString)
  case s' of
    Nothing -> pure $ Left $ pack ("cannot parse opened file" ++ unpack str)
    Just r  -> do
      print r
      pure $ Right r

foreign import javascript interruptible
  "fileSave(new Blob([$2],{type:'application/json'}),{fileName:$1,extensions:['.holbert']}).then($c);"
  saveAs :: JSString -> JSString -> IO ()
foreign import javascript interruptible
  "fileOpenHelper().then($c);"
  fileOpenHelper :: IO JSString
