module IPE(ClosureMap, InfoTableProvMap(..), DCMap
                    , emptyInfoTableProvMap) where

import GhcPrelude

import Name
import SrcLoc
import DataCon
import UniqMap

-- | A map from a 'Name' to the best approximate source position that
-- name arose from.
type ClosureMap = UniqMap
                                        Name  -- The binding
                                        (String, RealSrcSpan, String) -- The best approximate source position.

-- | A map storing all the different uses of a specific data constructor and the
-- approximate source position that usage arose from.
type DCMap = UniqMap DataCon [(Int, Maybe (RealSrcSpan, String))]

data InfoTableProvMap = InfoTableProvMap
                          { provDC :: DCMap
                          , provClosure :: ClosureMap }

emptyInfoTableProvMap :: InfoTableProvMap
emptyInfoTableProvMap = InfoTableProvMap emptyUniqMap emptyUniqMap
