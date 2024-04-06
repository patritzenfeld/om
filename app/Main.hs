{-# language ApplicativeDo #-}
module Main (main) where

import Instance
import Control.Monad.Output



main :: IO ()
main = do
  a <- getResults output
  print (a German)




output :: OutputMonad m => LangM m
output = do
  image "a"
  assertion True $ indent $ image "h"
  paragraph $ translate $ do
    german "Sprich Deutsch!"
    english "Speak English!"
  pure ()

