{-# language ApplicativeDo #-}
module Main (main) where

import OM.Instance
import Control.Monad.Output
import Control.Monad.IO.Class(MonadIO(liftIO))



main :: IO ()
main = do
  a <- doubleConvert $ output True
  print a




output :: (MonadIO m, OutputMonad m) => Bool -> LangM m
output b = do
  image $=<< liftIO $ do
    print "Oops, I did an IO again..."
    pure "a"
  assertion b $ indent $ do
    paragraph $ translate $ do
      german "Abgebrochen!"
      english "Aborted!"
    pure ()
  paragraph $ translate $ do
    german "Sprich Deutsch!"
    english "Speak English!"
  pure ()


doubleConvert :: LangM (ReportT [Result] IO) -> IO [Result]
doubleConvert out = do
  putStrLn "before getting results"
  res <- getResults out
  putStrLn "after getting results"
  res2 <- getResults $ toOutputMonad $ res
  putStrLn "after back conversion"
  pure (res2)
