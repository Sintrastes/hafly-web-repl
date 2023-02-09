{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveFunctor, TypeApplications #-}

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
import Data.IORef
import Control.Monad.Free
import Data.Proxy
import Data.MultiMap (fromList)

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
     xtermJs (base <> htmlModule) processHaflyExpr

processHaflyExpr :: InterpreterContext -> T.Text -> IO (T.Text, InterpreterContext)
processHaflyExpr ctx text = 
     let expr = parseExprDef (traverseOps $ operatorDefs ctx) text
     in case expr of
            Right (x, xDef) -> do
                case flattenDyn <$> interpretRec ctx x xDef of
                    Left s -> pure $ (T.pack s, ctx)
                    Right dy -> return ("", addDef ctx x dy)
            Left err -> do
                case parseExpression (traverseOps $ operatorDefs ctx) text of
                    Left err -> pure $ (T.pack $ errorBundlePretty err, ctx)
                    Right exp -> do
                        case interpretIO ctx exp of
                            Just action -> do
                                action
                                return ("", ctx)
                            Nothing -> do
                                case interpret ctx exp of
                                    Left s -> pure $ (T.pack s, ctx)
                                    Right result -> do
                                        res <- tryShowRes ctx result
                                        pure (res, ctx)

tryShowRes :: InterpreterContext -> Dynamic -> IO T.Text
tryShowRes ctx@InterpreterContext {..} x = catch (do
    case toMaybe $ dispatched "show" $ lookup "show" (traverseExprs exprDefs) of
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

liftFrontend' :: (MonadSample t m, Prerender t m) => b -> Client m b -> m b
liftFrontend' d x = do
    res <- current <$> prerender (pure d) x
    sample res

xtermJs :: _ => InterpreterContext -> (InterpreterContext -> T.Text -> IO (T.Text, InterpreterContext)) -> m ()
xtermJs initialCtx processInput = elAttr "div" ("style" =: "width: min(50em, 50vw); margin: auto;") $ do
    postBuild <- delay 0.1 =<< getPostBuild

    ctx <- liftIO $ newIORef initialCtx

    el "h1" $
      text "Try Hafly"

    el "p" $ do
      elAttr "a" ("href" =: "https://github.com/Sintrastes/hafly") $ text "Hafly"
      text " is a dynamic embeddable scripting language designed to be embedded in Haskell projects."

    el "p" $ do
      text "Try it out in the interpreter below! If it does not show up immediately, you may need to refresh the page."

    el "div" $ 
        elAttr "div" ("id" =: "terminal") blank

    let callback = function $ \_ _ args -> do
          arg <- valToText $ args Prelude.!! 0
          let continuation = args Prelude.!! 1

          currentCtx <- liftIO $ readIORef ctx

          result <- liftIO (processInput currentCtx arg)

          call continuation global [textToJSString $ fst result]

          liftIO $ writeIORef ctx (snd result)

          pure ()

    performEvent $ postBuild <&> \_ -> do
        liftJSM $ jsg1 ("initTerm" :: String)
            callback
        pure ()

    blank

data HTMLAlg a = 
    P String a 
  | H1 String a 
  | H2 String a 
  | Div (HTML ()) a
    deriving(Functor)

type HTML = Free HTMLAlg

p :: String -> HTML ()
p x = liftF $ P x ()

h1 :: String -> HTML ()
h1 x = liftF $ H1 x ()

h2 :: String -> HTML ()
h2 x = liftF $ H2 x ()

div :: HTML () -> HTML ()
div x = liftF $ Div x ()

htmlModule = InterpreterContext {
    operatorDefs = [],
    exprDefs = fromList [
      ("p", \_ -> toDyn p)
    , ("h1", \_ -> toDyn h1)
    , ("h2", \_ -> toDyn h2)
    , ("div", \_ -> toDyn Main.div)
    ],
    monadDefs = [
      fromMonad $ Proxy @HTML
    ]
}
