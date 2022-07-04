{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom.Core
import Language.Javascript.JSaddle.Warp
import Language.Javascript.JSaddle
import Data.Functor
import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.Text as T

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
     xtermJs T.toUpper

xtermJs :: _ => (T.Text -> T.Text) -> m ()
xtermJs processInput = do
     el "div" $ 
          elAttr "div" ("id" =: "terminal") blank

     let callback = function $ \_ _ args -> do
          arg <- valToText $ args Prelude.!! 0
          let continuation = args Prelude.!! 1

          let result = textToJSString $ 
               processInput arg

          call continuation global [result]

          pure ()

     prerender (pure ()) $ do
          liftJSM $ jsg1 ("initTerm" :: String)
               callback
          pure ()

     blank
