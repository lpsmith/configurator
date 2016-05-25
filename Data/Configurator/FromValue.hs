-- |
-- Module:      Data.Configurator.FromValue
-- Copyright:   (c) 2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>

module Data.Configurator.FromValue
     ( FromValue
     , ValueParser
     , optionalValue
     , Control.Monad.Reader.ask
     ) where

import Control.Monad.Reader
import Data.Configurator.FromValue.Implementation

class FromValue a where
    fromValue :: ValueParser a

instance FromValue a => FromValue (Maybe a) where
    fromValue = optionalValue fromValue

optionalValue :: ValueParser a -> ValueParser (Maybe a)
optionalValue m =
    ValueParser $ \mv -> case mv of
        Nothing -> (Just Nothing, mempty)
        _       -> case unValueParser m mv of
                     (Just res, w) -> (Just (Just res), w)
                     (Nothing,  w) -> (Nothing, w)
