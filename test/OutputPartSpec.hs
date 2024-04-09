{-# language StandaloneDeriving #-}
{-# language DeriveGeneric #-}
module OutputPartSpec (spec) where


import GHC.Generics
import Generic.Random
import Test.Hspec (describe, it, Spec)
import Test.QuickCheck (Arbitrary(..), chooseInt, forAll, vectorOf, Gen)
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import Control.Monad.Output

import qualified Data.Map as Map

import OM.Instance


instance BaseCase OutputPart where
  baseCase = do
    which <- chooseInt (1,3)
    case which of
      1 -> Latex <$> arbitrary
      2 -> Translated <$> arbitrary
      3 -> Code <$> arbitrary
      _ -> error "impossible constructor"


instance {-# Overlapping #-} Arbitrary (Map.Map Language String) where
  arbitrary = do
    texts <- vectorOf 3 arbitrary
    let langs = [minBound .. maxBound]
    pure $ Map.fromList $ zip langs texts


instance Arbitrary OutputPart where
  arbitrary = genericArbitraryU'



spec :: Spec
spec = do
    describe "language maps" $
        it "always contains all languages" $
             forAll (arbitrary :: Gen (Map.Map Language String)) $
              \langMap -> all (`Map.member` langMap) ([minBound .. maxBound] :: [Language])

    describe "conversion" $
        it "converting twice yields original value" $
             forAll arbitrary $
              \outputParts -> monadicIO $ do
                                             new <- run $ getResults (toOutputMonad outputParts)
                                             assert (new == outputParts)
