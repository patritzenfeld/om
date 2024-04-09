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
    | Enumerated ([OutputPart],[OutputPart])
    | Itemized [OutputPart]
    | Indented [OutputPart]
    | Translated (Map Language String)
    | Code (Map Language String)
    | Latex String
    deriving (Eq, Generic, Read, Show)


instance Monad m => GenericOutputMonad Language (ReportT [OutputPart] m) where
  assertion b = unless b . refuse
  -- | for printing a single image from file
  image = format . (:[]) . Image
  -- | for printing multiple images using the given map
  images descF fileF = format . (:[]) . Images . Map.mapKeys descF . Map.map fileF
  -- | for a complete paragraph
  paragraph = alignOutput ((:[]) . Paragraph . concat)
  -- | should abort at once
  refuse = alignOutput ((:[]) . Refuse . concat)
  -- | for an enumerated sequence of elements
  enumerateM f tups = combineReports ((:[]) . Enumerated . toTuple) combine  -- Somewhat hacky , didn't want to import a bunch of Monad.Transformers here
    where
      combine = map ( uncurry (combineTwoReports
                                (\x y -> [Enumerated (concat x, concat y)])
                              )
                    . first f)
                    tups

      toTuple res = (concatMap fst comp, concatMap snd comp)
        where
          comp = map expose $ concat $ concat res

          expose (Enumerated tup) = tup
          expose _                = error "This isn't possible"

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



toInterface :: OutputMonad m => OutputPart -> LangM m
toInterface res = case res of
  Paragraph xs     -> paragraph $ for_ xs toInterface
  Image path       -> image path
  Images m         -> images id id m
  Refuse xs        -> refuse $ for_ xs toInterface
  Enumerated tup   -> enumerateM id $ uncurry zip $ both (map toInterface) tup
  Itemized xs      -> itemizeM $ map toInterface xs
  Indented xs      -> indent $ for_ xs toInterface
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


getResults :: (Monad m,  Monoid n) => LangM (ReportT n m) -> m n
getResults lm = snd <$> runLangMReportMultiLang mempty (<>) ($ English) lm


getResultsWithRating :: (Monad m, Monoid n) => Rated (ReportT n m) -> m (Maybe Rational,n)
getResultsWithRating = runLangMReportMultiLang mempty (<>) ($ English)
