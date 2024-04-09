{-# language FlexibleInstances #-}
{-# language DeriveGeneric #-}
module OM.Instance (getResults, getResultsWithRating, toOutputMonad, OutputPart(..)) where


import Data.Map (Map)
import Data.Foldable
import Data.Tuple.Extra (second, dupe, first, both)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad.Output.Generic
import Control.Monad.Output
import Control.Monad
import Control.Monad.Trans.State (put, State)
import GHC.Generics (Generic)


data OutputPart
    = Paragraph [OutputPart]
    | Image FilePath
    | Images (Map String FilePath)
    | Refuse [OutputPart]
    | Enumerated [([OutputPart],[OutputPart])]
    | Itemized [[OutputPart]]
    | Indented [OutputPart]
    | Translated (Map Language String)
    | Code (Map Language String)
    | Latex String
    deriving (Eq, Generic, Read, Show)


instance GenericOutputMonad Language (ReportT OutputPart IO) where
  assertion b = unless b . refuse
  -- | for printing a single image from file
  image = format . Image
  -- | for printing multiple images using the given map
  images descF fileF = format . Images . Map.mapKeys descF . Map.map fileF
  -- | for a complete paragraph
  paragraph = alignOutput Paragraph
  -- | should abort at once
  refuse = alignOutput Refuse
  -- | for an enumerated sequence of elements
  enumerateM f tups = combineReports (Enumerated . toList) combine  -- Somewhat hacky , didn't want to import a bunch of Monad.Transformers here
    where
      combine :: [GenericLangM Language (GenericReportT Language OutputPart IO) ()]
      combine = map (\(a,b) -> (combineTwoReports (\x y -> Enumerated [(x, y)]) a b)) $ map (first f) tups

      toList :: [[OutputPart]] -> [([OutputPart],[OutputPart])]
      toList res = concatMap expose $ concat $ res

      expose :: OutputPart -> [([OutputPart],[OutputPart])]
      expose (Enumerated list) = list
      expose _                 = error "This isn't possible"

  -- | for an unenumerated sequence of elements
  itemizeM = combineReports Itemized
  -- | for indentation
  indent = alignOutput Indented
  -- | for LaTeX-Math code (i.e. without surrounding @$@)
  latex = format . Latex
  -- | for fixed width fonts (i.e. typewriter style)
  -- | same as 'code', but with different translations
  translatedCode =  format . Code . toMap
  -- | for displaying text with translations
  translated = format . Translated . toMap



toInterface :: OutputMonad m => OutputPart -> LangM m
toInterface res = case res of
  Paragraph xs     -> paragraph $ toOutputMonad xs
  Image path       -> image path
  Images m         -> images id id m
  Refuse xs        -> refuse $ toOutputMonad xs
  Enumerated list  -> enumerateM id $ zip (map (toOutputMonad . fst) list) (map (toOutputMonad . snd) list)
  Itemized xs      -> itemizeM $ map toOutputMonad xs
  Indented xs      -> indent $ toOutputMonad xs
  Translated m     -> translate $ put m
  Code m           -> translateCode $ put m
  Latex s          -> latex s



translateCode :: OutputMonad m => State (Map Language String) a -> LangM m
translateCode xs = translatedCode $ \l ->
  fromMaybe "" $ Map.lookup l $ translations xs

toOutputMonad :: OutputMonad m => [OutputPart] -> LangM m
toOutputMonad res = for_ res toInterface


toMap :: (Bounded l, Enum l, Ord l) => (l -> o) -> Map l o
toMap f = Map.fromList $ map (second f . dupe) [minBound .. maxBound]


getResults :: Monad m => LangM (ReportT OutputPart m) -> m [OutputPart]
getResults lm = (unParagraph . snd) <$> runLangMReportMultiLang (Paragraph []) (combine) ($ English) lm


unParagraph :: OutputPart -> [OutputPart]
unParagraph (Paragraph xs) = xs
unParagraph  _ = []

combine :: OutputPart -> OutputPart -> OutputPart
combine (Paragraph xs) x = Paragraph (xs ++ [x])
combine  _ _ = error "this is impossible"


getResultsWithRating :: Monad m => Rated (ReportT OutputPart m) -> m (Maybe Rational,[OutputPart])
getResultsWithRating lm = second unParagraph <$> runLangMReportMultiLang (Paragraph []) (combine) ($ English) lm
