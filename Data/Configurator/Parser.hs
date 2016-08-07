{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, ViewPatterns, TupleSections   #-}

-- |
-- Module:      Data.Configurator.Parser
-- Copyright:   (c) 2015 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- A set of combinators for high-level configuration parsing.

module Data.Configurator.Parser
    ( ConfigParser
    , ConfigParserA
    , ConfigParserM
    , ConfigError (..)
    , ConfigErrorLocation (..)
    , ConversionError (..)
    , ConversionErrorWhy (..)
    , Config
    , ConfigTransform
    , unsafeBind
    , runParser
    , runParserA
    , runParserM
    , parserA
    , parserM
    , subassocs
    , subgroups
    , localConfig
    , union
    , subconfig
    , superconfig
    , recover
    , key
    , keyWith
    ) where

import           Prelude hiding (null)

import           Control.Applicative hiding (optional, empty)
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.Monoid(Monoid(..),(<>))
import           Data.Configurator.Config
                   ( Config )
import           Data.Configurator.Types.Internal hiding (Group)
import           Data.Configurator.FromValue
                   ( FromMaybeValue(fromMaybeValue)
                   , MaybeParser
                   , runMaybeParser
                   )
import qualified Data.Configurator.Config as C
import qualified Data.Configurator.Config.Internal as CI
import           Data.Configurator.Parser.Implementation

#if __GLASGOW_HASKELL__ >= 800
{-# DEPRECATED unsafeBind "Use the ApplicativeDo language extension instead" #-}
#endif

-- |  The purpose of this function is to make it convenient to use do-notation
--    with 'ConfigParserA',  either by defining a Monad instance or locally
--    rebinding '(>>=)'.    Be warned that this is an abuse,  and incorrect
--    usage can result in exceptions.   A safe way to use this function
--    would be to treat is as applicative-do notation.  A safer alternative
--    would be to use the @ApplicativeDo@ language extension available in
--    GHC 8.0 and not use this function at all.

unsafeBind :: ConfigParserA a -> (a -> ConfigParserA b) -> ConfigParserA b
unsafeBind m k = ConfigParserA $ \r ->
                   case unConfigParserA m r of
                     (Nothing, w) -> let (_, w')  = unConfigParserA (k err) r
                                      in (Nothing, w <> w')
                     (Just a,  w) -> let (mb, w') = unConfigParserA (k a) r
                                      in (mb, w <> w')
  where err = error "unsafeBind on ConfigParserA used incorrectly"

runParser :: ConfigParser m => m a -> Config -> (Maybe a, [ConfigError])
runParser m conf = let (ma, errs) = unConfigParser_ m conf
                    in (ma, toErrors errs)

subassocs :: ConfigParser m => Name -> m [(Name, Value)]
subassocs t = configParser_ (\c -> (Just (C.subassocs t c), mempty))

subgroups :: ConfigParser m => Name -> m [Name]
subgroups t = configParser_ (\c -> (Just (C.subgroups t c), mempty))


-- |  Modifies the 'Config' that a subparser is operating on.
--    This is perfectly analogous to 'Control.Monad.Reader.local'.

localConfig :: ConfigParser m => ConfigTransform -> m a -> m a
localConfig f m = configParser_ (\r -> unConfigParser_ m (interpConfigTransform f r))

runParserA :: ConfigParserA a -> Config -> (Maybe a, [ConfigError])
runParserA = runParser

runParserM :: ConfigParserM a -> Config -> (Maybe a, [ConfigError])
runParserM = runParser

parserM :: ConfigParser m => ConfigParserM a -> m a
parserM (ConfigParserM m) = configParser_ m

parserA :: ConfigParser m => ConfigParserA a -> m a
parserA (ConfigParserA m) = configParser_ m

recover :: ConfigParser m => m a -> m (Maybe a)
recover m = configParser_ $ \r -> let (ma, errs) = unConfigParser_ m r
                                   in (Just ma, errs)

key :: (ConfigParser m, FromMaybeValue a) => Name -> m a
key name = keyWith name fromMaybeValue

keyWith :: (ConfigParser m) => Name -> MaybeParser a -> m a
keyWith name parser =
    configParser_ $ \(CI.Config c) ->
        case CI.lookupWithName name c of
          Nothing ->
              convert (KeyMissing (DL.toList (getLookupPlan name c))) Nothing
          Just (name', v) ->
              convert (Key "" name') (Just v)
  where
    convert loc mv =
        case runMaybeParser parser mv of
          (Nothing, errs) ->
              (Nothing, singleError (ConfigError loc (Just errs)))
          (Just a, []) ->
              (Just a, mempty)
          (Just a, errs@(_:_)) ->
              (Just a,  singleError (ConfigError loc (Just errs)))

getLookupPlan :: Name -> CI.ConfigPlan a -> DList Name
getLookupPlan = CI.foldPlan DL.empty (<>) (\k _ -> DL.singleton k)
