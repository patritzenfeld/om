{-# language ApplicativeDo #-}
module Main (main) where


import Control.Monad.Output
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Map               (fromList)

import OM.Instance            (OutputPart, getResults)




main :: IO ()
main = do
  x <- getResults $ output True
  print (x :: [OutputPart])



output :: (MonadIO m, OutputMonad m) => Bool -> LangM m
output b = do
  enumerateM (code . show) [(1 :: Int,code "Item 1"),(2,code "Item 2")]
  image $=<< liftIO $ do
    putStrLn "Oops, I did an IO again..."
    pure "a"
  images (\s -> "fig. " ++ s ++ "'s Picture") (++ ".png")
         $ fromList [("1: Pablo Picasso","pp"),("2: Vincent Van Gogh","vv"),("3: Salvador Dali","sd")]
  assertion b $ indent $ do
    paragraph $ translate $ do
      german "Abgebrochen!"
      english "Aborted!"
    pure ()
  paragraph $ translate $ do
    german "Sprich Deutsch!"
    english "Speak English!"
  pure ()
