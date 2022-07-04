{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom.Core
import Language.Javascript.JSaddle.Warp
import Language.Javascript.JSaddle
import Data.Functor
import Control.Concurrent
import Control.Monad.IO.Class

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
main = run 3911 $ mainWidgetWithHead header $ do
     postBuild <-  getPostBuild
     el "p" $ text "Hello, reflex!"

     el "div" $ 
          elAttr "div" ("id" =: "terminal") blank

     prerender (pure ()) $ do
          liftJSM $ jsg0 ("initTerm" :: String)
          pure ()

     blank
