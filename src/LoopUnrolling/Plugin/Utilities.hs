module LoopUnrolling.Plugin.Utilities where

import GhcPlugins

import Control.Monad
import Data.Data
import Data.Maybe
import Data.Word

type AnnoMap = UniqFM [[Word8]]

loadAnnotations :: ModGuts -> CoreM AnnoMap
loadAnnotations = getAnnotations id


annotationsOn :: Data a => AnnoMap -> CoreBndr -> [a]
annotationsOn ufm
  = fmap deserializeWithData . join . maybeToList . lookupUFM ufm

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

mkCloneId :: CoreBndr -> CoreM CoreBndr
mkCloneId b = do
  u <- getUniqueM
  pure $ mkUserLocal (nameOccName $ idName b) u (idType b) (getSrcSpan (idName b))
