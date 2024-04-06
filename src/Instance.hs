{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
module Instance (getResults) where


import Data.Map (Map)
import Data.Tuple.Extra (second, dupe)
import qualified Data.Map as Map
import Control.Monad.Output.Generic
import Control.Monad.Output
import Control.Monad


data Result
    = Empty
    | Paragraph [Result]
    | Image
    | Enumerated
    | Itemized
    | Indented [Result]
    | Translated (Map Language String)
    | Code (Map Language String)
    | Latex String
    | Results Result Result
    deriving (Show,Read,Eq)


instance GenericOutputMonad Language (ReportT Result IO) where
  assertion b r = unless b $ toAbort r
  -- | for printing a single image from file
  image _ = format Image
  -- | for printing multiple images using the given map
  images _ _ _ = format Image
  -- | for a complete paragraph
  paragraph r = alignOutput Paragraph r
  -- | should abort at once
  refuse r = toAbort r
  -- | for an enumerated sequence of elements
  enumerateM _ _ = format Enumerated
  -- | for an unenumerated sequence of elements
  itemizeM _ = format Itemized
  -- | for indentation
  indent = alignOutput Indented
  -- | for LaTeX-Math code (i.e. without surrounding @$@)
  latex = format . Latex
  -- | for fixed width fonts (i.e. typewriter style)
  -- | same as 'code', but with different translations
  translatedCode =  format . Code . toMap
  -- | for displaying text with translations
  translated = format . Translated . toMap


toMap :: (Bounded l, Enum l, Ord l) => (l -> o) -> Map l o
toMap f = Map.fromList $ map (second f . dupe) [minBound .. maxBound]

getResults :: LangM (ReportT Result IO) -> IO (Language -> Result)
getResults lm = snd <$> runLangMReport Empty (\a b -> Results a b) lm