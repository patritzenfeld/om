{-# language ApplicativeDo #-}
module Main (main) where

import OM.Instance
import Control.Monad.Output
import Control.Monad.IO.Class(MonadIO(liftIO))
import qualified Data.Map as Map


main :: IO ()
main = do
  x <- getResults $ toOutputMonad outputParts
  print (x :: [OutputPart])




output :: (MonadIO m, OutputMonad m) => Bool -> LangM m
output b = do
  enumerateM (\n -> code $ show n) [(1 :: Int,code "Item 1"),(2,code "Item 2")]
  image $=<< liftIO $ do
    putStrLn "Oops, I did an IO again..."
    pure "a"
  images (\s -> "fig. " ++ s ++ "'s Picture") (\n -> n ++ ".png")
         $ Map.fromList [("1: Pablo Picasso","pp"),("2: Vincent Van Gogh","vv"),("3: Salvador Dali","sd")]
  assertion b $ indent $ do
    paragraph $ translate $ do
      german "Abgebrochen!"
      english "Aborted!"
    pure ()
  paragraph $ translate $ do
    german "Sprich Deutsch!"
    english "Speak English!"
  pure ()


outputParts :: [OutputPart]
outputParts =
    [ Itemized
       [
         [Translated (Map.fromList [(German, "f")])]
       ]
    ]