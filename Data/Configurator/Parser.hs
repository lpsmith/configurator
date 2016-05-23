{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, ViewPatterns, TupleSections   #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable           #-}

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
    , ConfigErrorWhy (..)
    , unsafeBind
    , runParser
    , runParserA
    , runParserM
    , parserA
    , parserM
    , askConfig
    , localConfig
    , recover
    , required
    , requiredPred
    , optional
    , optionalPred
    , parseField
    ) where

import           Prelude hiding (null)

import           Control.Applicative hiding (optional)
import           Data.Configurator.Types.Internal hiding (Group)
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.Monoid(Monoid(..),(<>))
import           Data.Typeable
import           Data.Configurator.Config
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
                    in (ma, DL.toList errs)

-- |  Returns the current 'Config' that the parser is operating on.
--    This is perfectly analogous to 'Control.Monad.Reader.ask', however,
--    'ConfigParserA' cannot be an instance of
--    'Control.Monad.Reader.MonadReader' because of the 'Monad' constraint.
--    (Which is too strict of a constraint,  if you look past the name
--    of the class.)

askConfig :: ConfigParser m => m Config
askConfig =  configParser_ (\r -> (Just r, mempty))

-- |  Modifies the 'Config' that a subparser is operating on.
--    This is perfectly analogous to 'Control.Monad.Reader.local'.

localConfig :: ConfigParser m => (Config -> Config) -> m a -> m a
localConfig f m = configParser_ (\r -> unConfigParser_ m (f r))

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

required :: (ConfigParser m, Configured a, Typeable a) => Name -> m a
required name = requiredPred name (const True)

requiredPred :: (ConfigParser m, Configured a, Typeable a)
             => Name -> (a -> Bool) -> m a
requiredPred name p = parseField name Nothing Nothing p

optional :: (ConfigParser m, Configured a, Typeable a, Show a)
         => Name -> a -> m a
optional name def = optionalPred name def (const True)

optionalPred :: (ConfigParser m, Configured a, Typeable a, Show a)
             => Name -> a -> (a -> Bool) -> m a
optionalPred name def p = parseField name (Just def) (Just (show def)) p

parseField :: forall m a. (ConfigParser m, Configured a, Typeable a)
           => Name -> Maybe a -> Maybe String -> (a -> Bool) -> m a
parseField name mdef mdefstr p =
    configParser_ $ \c ->
        case lookupWithName name c of
          Nothing -> (mdef, DL.singleton (miss_err c))
          Just (name', v) ->
              case convert v of
                Nothing -> (mdef, DL.singleton (conv_err name' v))
                Just v' -> if p v'
                           then (Just v', mempty)
                           else (mdef, DL.singleton (pred_err name' v))
  where
     miss_err c
         = ConfigError {
             configErrorKeys = DL.toList (getLookupPlan name c),
             configErrorVal  = Nothing,
             configErrorType = typeOf (undefined :: a),
             configErrorDef  = mdefstr,
             configErrorWhy  = Missing
           }
     conv_err name' v
         = ConfigError {
             configErrorKeys = [name'],
             configErrorVal  = Just v,
             configErrorType = typeOf (undefined :: a),
             configErrorDef  = mdefstr,
             configErrorWhy  = ConversionError
           }
     pred_err name' v
         = ConfigError {
             configErrorKeys = [name'],
             configErrorVal  = Just v,
             configErrorType = typeOf (undefined :: a),
             configErrorDef  = mdefstr,
             configErrorWhy  = PredicateFailed
           }

getLookupPlan :: Name -> ConfigPlan a -> DList Name
getLookupPlan = foldPlan DL.empty (<>) (\k _ -> DL.singleton k)
