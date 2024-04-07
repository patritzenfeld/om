{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
module OM.Instance (getResults, getResultsWithRating, toOutputMonad, Result(..)) where


import Data.Map (Map)
import Data.Foldable
import Data.Tuple.Extra (second, dupe)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad.Output.Generic
import Control.Monad.Output
import Control.Monad
import Control.Monad.Trans.State (put, State)


data Result
    = Paragraph [Result]
    | Image FilePath
    | Enumerated
    | Itemized [Result]
    | Indented [Result]
    | Translated (Map Language String)
    | Code (Map Language String)
    | Latex String
    deriving (Show,Read,Eq)


instance Monad m => GenericOutputMonad Language (ReportT [Result] m) where
  assertion b = unless b . refuse
  -- | for printing a single image from file
  image = format . (:[]) . Image
  -- | for printing multiple images using the given map
  images _ _ _ = format [Image "no"]
  -- | for a complete paragraph
  paragraph = alignOutput ((:[]) . Paragraph . concat)
  -- | should abort at once
  refuse = toAbort
  -- | for an enumerated sequence of elements
  enumerateM _ _ = format [Enumerated]
  -- | for an unenumerated sequence of elements
  itemizeM = combineReports ((:[]) . Itemized . concat . concat)
  -- | for indentation
  indent = alignOutput ((:[]) . Indented . concat)
  -- | for LaTeX-Math code (i.e. without surrounding @$@)
  latex = format . (:[]) . Latex
  -- | for fixed width fonts (i.e. typewriter style)
  -- | same as 'code', but with different translations
  translatedCode =  format . (:[]) . Code . toMap
  -- | for displaying text with translations
  translated = format . (:[]) . Translated . toMap



toInterface :: OutputMonad m => Result -> LangM m
toInterface res = case res of
  Paragraph xs  -> paragraph $ for_ xs toInterface
  Image path    -> image path
  Enumerated    -> error "rip"
  Itemized xs   -> itemizeM $ map toInterface xs
  Indented xs   -> indent $ for_ xs toInterface
  Translated m  -> translate $ put m
  Code m        -> translateCode $ put m
  Latex s       -> latex s



translateCode :: OutputMonad m => State (Map Language String) a -> LangM m
translateCode xs = translatedCode $ \l ->
  fromMaybe "" $ Map.lookup l $ translations xs

toOutputMonad :: OutputMonad m => [Result] -> LangM m
toOutputMonad res = for_ res toInterface


toMap :: (Bounded l, Enum l, Ord l) => (l -> o) -> Map l o
toMap f = Map.fromList $ map (second f . dupe) [minBound .. maxBound]


getResults :: LangM (ReportT [Result] IO) -> IO [Result]
getResults lm = snd <$> runLangMReportMultiLang [] (++) ($ English) lm


getResultsWithRating :: Rated (ReportT [Result] IO) -> IO (Maybe Rational,[Result])
getResultsWithRating lm = runLangMReportMultiLang [] (++) ($ English) lm
