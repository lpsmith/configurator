-- |
-- Module:      Data.Configurator.Config.Internal
-- Copyright:   (c) 2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>

module Data.Configurator.Config
     ( Config
     , lookup
     , lookupWithName
     , subgroups
     , subassocs
     ) where

import Prelude hiding (lookup)
import Data.Configurator.Types(Name,Value)
import Data.Configurator.Config.Implementation(Config(..))
import qualified Data.Configurator.Config.Implementation as C

lookup :: Name -> Config -> Maybe Value
lookup k (Config c) = C.lookup k c

lookupWithName :: Name -> Config -> Maybe (Name, Value)
lookupWithName k (Config c) = C.lookupWithName k c

subgroups :: Name -> Config -> [Name]
subgroups k (Config c) = C.subgroups k c

subassocs :: Name -> Config -> [(Name,Value)]
subassocs k (Config c) = C.subassocs k c
