{-# language ApplicativeDo #-}
module Main (main) where

import Instance
import Control.Monad.Output



main :: IO ()
main = do
  a <- doubleConvert output
  print a




output :: OutputMonad m => LangM m
output = do
  image "a"
  assertion True $ indent $ image "h"
  paragraph $ translate $ do
    german "Sprich Deutsch!"
    english "Speak English!"
  pure ()


doubleConvert :: LangM (ReportT Result IO) -> IO Result
doubleConvert out = do
  res <- getResults out
  res2 <- getResults $ toOutputMonad $ res German
  pure (res2 German)