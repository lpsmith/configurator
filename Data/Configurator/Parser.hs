{-# LANGUAGE OverloadedStrings, ScopedTypeVariables    #-}
{-# LANGUAGE BangPatterns, ViewPatterns, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable         #-}

-- |
-- Module:      Data.Configurator.Parser
-- Copyright:   (c) 2015 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- A set of combinators for high-level configuration parsing.

module Data.Configurator.Parser where

import           Prelude hiding (null)

import           Control.Applicative
import           Control.Monad (ap)
import           Data.Configurator.Types.Internal hiding (Group)
-- import           Data.CritBit.Map.Lazy (CritBit)
-- import qualified Data.CritBit.Map.Lazy as CB
import           Data.DList (DList)
import qualified Data.DList as DL
-- import           Data.Function (on)
-- import qualified Data.List.Ordered as OL
-- import           Data.Maybe(mapMaybe)
import           Data.Monoid(Monoid(..),(<>))
-- import           Data.Text(Text)
-- import qualified Data.Text as T
import           Data.Typeable

import           Data.Configurator.ConfigMap


data ConfigParseError = ConfigParseError {
      configErrorKeys :: ![Name]
    , configErrorVal  :: !(Maybe Value)
    , configErrorType :: !TypeRep
    , configErrorDef  :: !(Maybe String)
    , configErrorWhy  :: !ConfigErrorWhy
    } deriving (Eq, Show)

data ConfigErrorWhy
    = Missing
    | ConversionError
    | PredicateFailed
      deriving (Eq, Ord, Show, Enum, Bounded)

type RMW r w a = r -> (Maybe a, w)


-- | A @'ConfigParserM' a@ computation produces a value of type @'Maybe' a@
--   from a given 'Config',  in addition to a list of diagnostic messages
--   which may be interpreted as warnings or errors as deemed appropriate.
--   Errors are cre

newtype ConfigParserM a
    = ConfigParserM { unConfigParserM :: RMW Config (DList ConfigParseError) a }
      deriving (Typeable, Functor)

instance Applicative ConfigParserM where
    pure  = return
    (<*>) = ap

instance Monad ConfigParserM where
    return a = ConfigParserM $ \_ -> (pure a, mempty)
    m >>= k  = ConfigParserM $
                   \r -> let (ma, w ) = unConfigParserM m r
                          in case ma of
                               Nothing -> (Nothing, w)
                               Just a  -> let (mb, w') = unConfigParserM (k a) r
                                           in (mb, w <> w')

-- | After an error,  actions of type 'ConfigParserA' will continue to
--   run in order to produce more error messages.

newtype ConfigParserA a
    = ConfigParserA { unConfigParserA :: RMW Config (DList ConfigParseError) a }
      deriving (Typeable, Functor)

instance Applicative ConfigParserA where
    pure a  = ConfigParserA $ \_ -> (pure a, mempty)
    f <*> a = ConfigParserA $ \r -> let (mf, w ) = unConfigParserA f r
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

-- |  The purpose of this function is to make it convenient to use do-notation
--    with 'ConfigParserA',  either by defining a Monad instance or locally
--    rebinding '(>>=)'.    Be warned that this is an abuse,  and incorrect
--    usage can result in exceptions.   A safe way to use this function
--    would be to treat is as applicative-do notation.  A safer alternative
--    would be to use the @applicative-quoters@ package and not use this
--    function at all.

unsafeBind :: ConfigParserA a -> (a -> ConfigParserA b) -> ConfigParserA b
unsafeBind m k = ConfigParserA $ \r ->
                   case unConfigParserA m r of
                     (Nothing, w) -> let (_, w')  = unConfigParserA (k err) r
                                      in (Nothing, w <> w')
                     (Just a,  w) -> let (mb, w') = unConfigParserA (k a) r
                                      in (mb, w <> w')
  where err = error "unsafeBind on ConfigParserA used incorrectly"

class Applicative m => ConfigParser m where
    configParser_   :: RMW Config (DList ConfigParseError) a -> m a
    unConfigParser_ :: m a -> RMW Config (DList ConfigParseError) a

instance ConfigParser ConfigParserM where
    configParser_   = ConfigParserM
    unConfigParser_ = unConfigParserM

instance ConfigParser ConfigParserA where
    configParser_   = ConfigParserA
    unConfigParser_ = unConfigParserA

runParser :: ConfigParser m => m a -> Config -> (Maybe a, [ConfigParseError])
runParser m conf = let (ma, errs) = unConfigParser_ m conf
                    in (ma, DL.toList errs)

runParserA :: ConfigParserA a -> Config -> (Maybe a, [ConfigParseError])
runParserA = runParser

runParserM :: ConfigParserM a -> Config -> (Maybe a, [ConfigParseError])
runParserM = runParser

localConfig :: ConfigParser m => (Config -> Config) -> m a -> m a
localConfig f m = configParser_ (\r -> unConfigParser_ m (f r))

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
         = ConfigParseError {
             configErrorKeys = DL.toList (getLookupPlan name c),
             configErrorVal  = Nothing,
             configErrorType = typeOf (undefined :: a),
             configErrorDef  = mdefstr,
             configErrorWhy  = Missing
           }
     conv_err name' v
         = ConfigParseError {
             configErrorKeys = [name'],
             configErrorVal  = Just v,
             configErrorType = typeOf (undefined :: a),
             configErrorDef  = mdefstr,
             configErrorWhy  = ConversionError
           }
     pred_err name' v
         = ConfigParseError {
             configErrorKeys = [name'],
             configErrorVal  = Just v,
             configErrorType = typeOf (undefined :: a),
             configErrorDef  = mdefstr,
             configErrorWhy  = PredicateFailed
           }

getLookupPlan :: Name -> ConfigPlan a -> DList Name
getLookupPlan = foldPlan DL.empty (<>) (\k _ -> DL.singleton k)
