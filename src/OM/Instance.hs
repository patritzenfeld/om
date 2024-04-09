{-# language DeriveGeneric #-}
module OM.Instance
    ( OutputPart(..)
    , getResults
    , getResultsWithRating
    , toOutputMonad
    ) where


import Control.Monad                (unless)
import Control.Monad.Output
import Control.Monad.Output.Generic (runLangMReportMultiLang)
import Control.Monad.Trans.State    (State, put)
import Data.Foldable
import Data.Map                     (Map)
import Data.Maybe                   (fromMaybe)
import Data.Tuple.Extra             (dupe, first, second)
import GHC.Generics                 (Generic)

import qualified Data.Map as Map



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


instance Monad m => GenericOutputMonad Language (ReportT OutputPart m) where
  -- | for assertions, i.e. expected behaviour is explanation
  -- (and abortion on 'False')
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
  enumerateM f tups = combineReports (Enumerated . concatMap expose . concat) combine  -- Somewhat hacky , didn't want to import a bunch of monad transformers here
    where
      combine = map (uncurry (combineTwoReports $ curry $ Enumerated . (:[])) . first f) tups

      expose (Enumerated list) = list
      expose _                 = error "This is impossible"
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
  Enumerated list  -> enumerateM id $ zip (map (toOutputMonad . fst) list)
                                          (map (toOutputMonad . snd) list)
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
getResults lm = unbox . snd <$> runLangMReportMultiLang (Paragraph []) gather ($ English) lm


unbox :: OutputPart -> [OutputPart]
unbox (Paragraph xs) = xs
unbox  _ = error "this is impossible"

gather :: OutputPart -> OutputPart -> OutputPart
gather (Paragraph xs) x = Paragraph (xs ++ [x])
gather  _ _ = error "this is impossible"


getResultsWithRating :: Monad m => Rated (ReportT OutputPart m) -> m (Maybe Rational,[OutputPart])
getResultsWithRating lm = second unbox <$> runLangMReportMultiLang (Paragraph []) gather ($ English) lm
