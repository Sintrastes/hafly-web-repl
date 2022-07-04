{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom.Core hiding (Dynamic)
import Language.Javascript.JSaddle.Warp
import Language.Javascript.JSaddle hiding (catch)
import Data.Functor
import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.Text as T
import Language.Hafly.Parser
import Language.Hafly.Stdlib
import Language.Hafly.Interpreter
import Text.Megaparsec.Error
import Control.Exception
import Data.Dynamic
import Data.MultiMap
import Prelude hiding (lookup)


header :: _ => m ()
header = do
  el "title" $ text "Hafly REPL"
  elAttr "meta" (
    "name" =: "viewport" <>
    "content" =: "width=device-width, initial-scale=1") blank
  elAttr "script" (
    "src" =: "https://sintrastes.github.io/demos/hafly_repl/node_modules/xterm/lib/xterm.js") blank
  elAttr "script" (
    "src" =: "https://sintrastes.github.io/demos/hafly_repl/main.js") blank
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/hafly_repl/node_modules/xterm/css/xterm.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank

main :: IO ()
main = run 3911 $ mainWidgetWithHead header $
     xtermJs processHaflyExpr

processHaflyExpr :: T.Text -> IO T.Text
processHaflyExpr text = 
     let expr = parseExprDef (operatorDefs base) text
     in case expr of
            Right (x, xDef) -> do
                case flattenDyn <$> interpretRec base x xDef of
                    Left s -> pure $ T.pack s
                    Right dy -> pure ""
            Left err -> do
                case parseExpression (operatorDefs base) text of
                    Left err -> pure $ T.pack $ errorBundlePretty err
                    Right exp -> do
                        case interpretIO base exp of
                            Just action -> do
                                action
                                return ""
                            Nothing -> do
                                case interpret base exp of
                                    Left s -> pure $ T.pack s
                                    Right result -> tryShowRes base result

tryShowRes :: InterpreterContext -> Dynamic -> IO T.Text
tryShowRes ctx@InterpreterContext {..} x = catch (do
    case toMaybe $ dispatched "show" $ lookup "show" exprDefs of
      Nothing -> pure $ T.pack $ show x
      Just showF -> do
        -- Need to guard against excepions for now as
        --  dynApply can throw.
        -- Would be better to model this with Either in the 
        --  future.
        result <- catch
            (evaluate $ maybe (pure $ T.pack $ show x) pure . fmap T.pack . asString . flattenDyn <$> flexibleDynApp showF x)
            (\(e :: SomeException) -> pure $ Left "")
        case result of
          Left s -> return $ T.pack $ show x
          Right r -> r)
            (\(e :: SomeException) -> pure $ T.pack $ show x)

xtermJs :: _ => (T.Text -> IO T.Text) -> m ()
xtermJs processInput = do
     el "div" $ 
          elAttr "div" ("id" =: "terminal") blank

     let callback = function $ \_ _ args -> do
          arg <- valToText $ args Prelude.!! 0
          let continuation = args Prelude.!! 1

          result <- textToJSString <$> liftIO
               (processInput arg)

          call continuation global [result]

          pure ()

     prerender (pure ()) $ do
          liftJSM $ jsg1 ("initTerm" :: String)
               callback
          pure ()

     blank
