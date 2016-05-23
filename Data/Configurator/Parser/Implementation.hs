{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module:      Data.Configurator.Parser.Implementation
-- Copyright:   (c) 2015-2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>

module Data.Configurator.Parser.Implementation where

import Control.Monad (ap)
import Data.Configurator.Config (Config)
import Data.Configurator.Types (ConfigError)
import Data.DList (DList)
import Data.Monoid
import Data.Typeable (Typeable)

type RMW r w a = r -> (Maybe a, w)

-- | A @'ConfigParserM' a@ computation produces a value of type @'Maybe' a@
--   from a given 'Config',  in addition to a list of diagnostic messages
--   which may be interpreted as warnings or errors as deemed appropriate.
--   Errors are cre

newtype ConfigParserM a
    = ConfigParserM { unConfigParserM :: RMW Config (DList ConfigError) a }
      deriving (Typeable, Functor)

instance Applicative ConfigParserM where
    pure  = return
    (<*>) = ap

instance Monad ConfigParserM where
    return a = ConfigParserM $ \_ -> (pure a, mempty)
    m >>= k  = ConfigParserM $ \r ->
                   let (ma, w ) = unConfigParserM m r
                    in case ma of
                         Nothing -> (Nothing, w)
                         Just a  -> let (mb, w') = unConfigParserM (k a) r
                                     in (mb, w <> w')

-- | After an error,  actions of type 'ConfigParserA' will continue to
--   run in order to produce more error messages.

newtype ConfigParserA a
    = ConfigParserA { unConfigParserA :: RMW Config (DList ConfigError) a }
      deriving (Typeable, Functor)

instance Applicative ConfigParserA where
    pure a  = ConfigParserA $ \_ -> (pure a, mempty)
    f <*> a = ConfigParserA $ \r ->
                  let (mf, w ) = unConfigParserA f r
                      (ma, w') = unConfigParserA a r
                   in (mf <*> ma, w <> w')

{--
--- There are at least three obvious "implementations" of <|> on ConfigParserM
--- TODO: check alternative laws and pick an appropriate instance for each

instance Alternative ConfigParserM where
    empty   = ConfigParserM $ \_ -> (Nothing, mempty)
    f <|> g = ConfigParserM $ \r ->
                  case unConfigParserM m0 r of
                    (Nothing, _errs0) -> unConfigParserM m1 r
                    res -> res

instance Alternative ConfigParserA where
    empty   = ConfigParserA $ \_ -> (Nothing, mempty)
    f <|> g = ConfigParserA $ \r -> let (mf, w ) = unConfigParserA f r
                                        (mg, w') = unConfigParserA g r
                                     in (mf <|> mg, w <> w')


instance Alternative ConfigParserA where
    empty   = ConfigParserA $ \_ -> (Nothing, mempty)
    f <|> g = ConfigParserA $ \r -> let (mf, w ) = unConfigParserA f r
                                        (mg, w') = unConfigParserA g r
                                     in case mf of
                                          (Just f) -> (mf, w)
                                          Nothing  -> (mg, w <> w')

--}

-- |  The 'ConfigParser' type class abstracts over 'ConfigParserM' and
--    'ConfigParserA'.   This is intended to be a closed typeclass, without
--    any additional instances.

class Applicative m => ConfigParser m where
    configParser_   :: RMW Config (DList ConfigError) a -> m a
    unConfigParser_ :: m a -> RMW Config (DList ConfigError) a

{--
--- Unfortunately,  this doesn't work (yet?) because of MonadReader's
--- Monad superclass.

instance ConfigParser m => MonadReader m where
     ask = configParser_ $ \c -> (Just c, mempty)


Data.Configurator.Parser.Internal

--}

instance ConfigParser ConfigParserM where
    configParser_   = ConfigParserM
    unConfigParser_ = unConfigParserM

instance ConfigParser ConfigParserA where
    configParser_   = ConfigParserA
    unConfigParser_ = unConfigParserA
