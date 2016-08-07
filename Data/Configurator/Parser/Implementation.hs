{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor #-}

-- |
-- Module:      Data.Configurator.Parser.Implementation
-- Copyright:   (c) 2015-2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>

module Data.Configurator.Parser.Implementation where

import Control.Monad (ap)
import Data.Configurator.Config (Config)
import qualified Data.Configurator.Config as C
import Data.Configurator.Config.Implementation (ConfigPlan(..))
import Data.Configurator.Types (ConfigError)
import Data.DList (DList)
import Data.Monoid
import Data.Text (Text)
import Data.Typeable (Typeable)

type RMW r w a = r -> (Maybe a, w)

type ConfigErrors = Maybe (DList ConfigError)

-- | A @'ConfigParserM' a@ computation produces a value of type @'Maybe' a@
--   from a given 'Config',  in addition to a list of diagnostic messages
--   which may be interpreted as warnings or errors as deemed appropriate.
--   Errors are cre

newtype ConfigParserM a
    = ConfigParserM { unConfigParserM :: RMW Config ConfigErrors a }
      deriving (Typeable, Functor)

instance Applicative ConfigParserM where
    pure a = ConfigParserM $ \_ -> (pure a, mempty)
    (<*>) = ap

instance Monad ConfigParserM where
#if !(MIN_VERSION_base(4,8,0))
    return = pure
#endif
    m >>= k  = ConfigParserM $ \r ->
                   let (ma, w ) = unConfigParserM m r
                    in case ma of
                         Nothing -> (Nothing, w)
                         Just a  -> let (mb, w') = unConfigParserM (k a) r
                                     in (mb, w <> w')

-- | After an error,  actions of type 'ConfigParserA' will continue to
--   run in order to produce more error messages.

newtype ConfigParserA a
    = ConfigParserA { unConfigParserA :: RMW Config ConfigErrors a }
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
    configParser_   :: RMW Config ConfigErrors a -> m a
    unConfigParser_ :: m a -> RMW Config ConfigErrors a

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

newtype ConfigTransform = ConfigTransform (ConfigPlan ())

instance Monoid ConfigTransform where
   mempty = ConfigTransform (ConfigPlan ())
   (ConfigTransform x) `mappend` (ConfigTransform y) = (ConfigTransform (go x))
     where
       go (ConfigPlan _)      = y
       go (Union a b)         = Union (go a) (go b)
       go (Superconfig pre a) = Superconfig pre (go a)
       go (Subconfig pre a)   = Subconfig pre (go a)
       go Empty               = Empty

union :: ConfigTransform -> ConfigTransform -> ConfigTransform
union (ConfigTransform x) (ConfigTransform y) = ConfigTransform (Union x y)

subconfig :: Text -> ConfigTransform -> ConfigTransform
subconfig k (ConfigTransform x) = ConfigTransform (Subconfig k x)

superconfig :: Text -> ConfigTransform -> ConfigTransform
superconfig k (ConfigTransform x) = ConfigTransform (Superconfig k x)

interpConfigTransform :: ConfigTransform -> Config -> Config
interpConfigTransform (ConfigTransform x) config = go x
  where
    go Empty             = C.empty
    go (ConfigPlan _)    = config
    go (Superconfig k x) = C.superconfig k (go x)
    go (Subconfig   k x) = C.subconfig k (go x)
    go (Union       x y) = C.union (go x) (go y)
