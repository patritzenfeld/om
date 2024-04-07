{-# language ApplicativeDo #-}
module Main (main) where

import OM.Instance
import Control.Monad.Output



main :: IO ()
main = do
  a <- doubleConvert $ output True
  print a




output :: OutputMonad m => Bool -> LangM m
output b = do
  image "a"
  assertion b $ indent $ do
    image "h"
    paragraph $ translate $ do
      german "Sprich Deutsch!"
      english "Speak English!"
    pure ()
  paragraph $ translate $ do
    german "Sprich Deutsch!"
    english "Speak English!"
  pure ()


doubleConvert :: LangM (ReportT [Result] IO) -> IO [Result]
doubleConvert out = do
  res <- getResults out
  res2 <- getResults $ toOutputMonad $ res German
  pure (res2 German)
