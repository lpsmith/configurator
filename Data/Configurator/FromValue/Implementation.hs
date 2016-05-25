{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor   #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module:      Data.Configurator.FromValue.Implementation
-- Copyright:   (c) 2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>

module Data.Configurator.FromValue.Implementation
     ( RMW
     , ValueParser(..)
     , runValueParser
     ) where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Configurator.Types(Value(..))
import           Data.DList (DList)
import qualified Data.DList as DList
import           Data.Monoid
import           Data.Typeable(Typeable)

type RMW r w a = r -> (Maybe a, w)

type ConversionError = ()

newtype ValueParser a = ValueParser {
      unValueParser :: RMW (Maybe Value) (DList ConversionError) a
    } deriving (Typeable, Functor)

runValueParser :: ValueParser a -> Maybe Value -> (Maybe a, [ConversionError])
runValueParser m mv =
    let (ma, errs) = unValueParser m mv
     in (ma, DList.toList errs)

instance Applicative ValueParser where
    pure a = ValueParser $ \_ -> (Just a, mempty)
    (<*>)  = ap

instance Alternative ValueParser where
    empty   = ValueParser $ \_  -> (Nothing, mempty)
    f <|> g = ValueParser $ \mv ->
                 case unValueParser f mv of
                   (Nothing, w) ->
                       case unValueParser g mv of
                         (Nothing  , w') -> (Nothing, w <> w')
                         res -> res
                   res -> res

instance Monad ValueParser where
#if !(MIN_VERSION_base(4,8,0))
    return = pure
#endif
    m >>= k  = ValueParser $ \mv ->
                   case unValueParser m mv of
                     (Just a, w)  -> let (mb, w') = unValueParser (k a) mv
                                      in (mb, w <> w')
                     (Nothing, w) -> (Nothing, w)

    -- FIXME: generate an informative error message
    fail _msg = empty

instance MonadReader (Maybe Value) ValueParser where
    ask       = ValueParser $ \mv -> (Just mv, mempty)
    local f m = ValueParser $ unValueParser m . f
#if MIN_VERSION_mtl(2,1,0)
    reader f  = ValueParser $ \mv -> (Just (f mv), mempty)
#endif
