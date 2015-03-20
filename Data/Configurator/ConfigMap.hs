{-# LANGUAGE OverloadedStrings, ScopedTypeVariables    #-}
{-# LANGUAGE BangPatterns, ViewPatterns, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable         #-}

module Data.Configurator.ConfigMap where

import           Prelude hiding ((++),null)
import           Control.Applicative
import           Control.Monad (ap)
-- import           Control.Arrow(first)
import           Data.Maybe(mapMaybe)
import           Data.Monoid(Monoid(..),(<>))
--import           Data.Ratio
--import           Data.ByteString (ByteString)
import           Data.Configurator.Types.Internal hiding (Group)
import           Data.Typeable
import           Data.CritBit.Map.Lazy (CritBit)
import qualified Data.CritBit.Map.Lazy as CB
import qualified Data.List.Ordered as OL
import           Data.Function (on)
import           Data.Text(Text)
import qualified Data.Text as T
import           Data.DList (DList)
import qualified Data.DList as DL
--import qualified Data.Text.Encoding as T
--import qualified Data.Text.Lazy as TL
--import qualified Data.Text.Lazy.Builder as TB
--import qualified Data.Text.Lazy.Builder.Int as TB
--import qualified Data.Text.Lazy.Builder.RealFloat as TB

data ConfigPlan a
    = Subconfig   Text (ConfigPlan a)
    | Group       Text (ConfigPlan a)
    | Union       (ConfigPlan a) (ConfigPlan a)
    | ConfigPlan  a
    | Empty
      deriving (Show, Typeable, Functor)

addPrefix :: Name -> Name -> Name
addPrefix pre key
    | T.null pre = key
    | T.null key = pre
    | otherwise  = T.concat [pre, ".", key]

stripPrefix :: Name -> Name -> Maybe Name
stripPrefix pre key = T.stripPrefix pre key >>= T.stripPrefix "."

foldPlan :: b -> (b -> b -> b) -> (Text -> a -> b) -> Text -> ConfigPlan a -> b
foldPlan empty union lookup = loop
  where
    loop key  (Subconfig  pre pl ) = loop (addPrefix pre key) pl
    loop key  (Group      pre pl ) = case stripPrefix pre key of
                                       Nothing   -> empty
                                       Just key' -> loop key' pl
    loop key  (Union      pl1 pl2) = loop key pl2 `union` loop key pl1
    loop key  (ConfigPlan a      ) = lookup key a
    loop _key  Empty               = empty
{-# INLINE foldPlan #-}


type ConfigMap a = ConfigPlan (CB.CritBit Text a)
type Config = ConfigMap Value

subassocs :: Text -> ConfigMap a -> [(Text,a)]
subassocs key c = subassocs_ ((CB.assocs .) . submap) key c

lookup :: Text -> ConfigMap a -> Maybe a
lookup = foldPlan Nothing (<|>) CB.lookup

lookupWithName :: Name -> ConfigMap a -> Maybe (Name,a)
lookupWithName = foldPlan Nothing (<|>) (\k m -> (k,) <$> CB.lookup k m)


subassocs_ :: (Text -> a -> [(Text,b)])
           -> Text -> ConfigPlan a -> [(Text,b)]
subassocs_ subassocs = loop
  where
    stripPrefixes pre = mapMaybe $ \(k,v) ->
                          case stripPrefix pre k of
                            Nothing -> Nothing
                            Just k' -> Just (k',v)

    addPrefixes pre = map (\(k,v) -> (addPrefix pre k,v))

    loop !_key Empty = []
    loop !key (Subconfig   pre pl) =
        stripPrefixes pre (loop (addPrefix pre key) pl)
    loop !key (Group pre pl) =
        case T.commonPrefixes pre key of
          Nothing
              | T.null pre -> loop key pl
              | T.null key -> addPrefixes pre (loop key pl)
              | otherwise  -> []
          Just (prefix, pre', key')
              | T.null key' -> addPrefixes pre    (loop T.empty pl)
              | T.null pre' -> case T.stripPrefix "." key' of
                                 Nothing -> []
                                 Just key'' -> addPrefixes prefix (loop key'' pl)
              | otherwise   -> []
    loop !key (Union pl1 pl2) =
        OL.unionBy (compare `on` fst) (loop key pl2) (loop key pl1)
    loop !key (ConfigPlan map) = subassocs key map

submap :: Text -> CritBit Text a -> CritBit Text a
submap key map
    | T.null key = map
    | otherwise  = let (_ , gt) = CB.split (key <> ".")  map
                       (lt, _ ) = CB.split (key <> ".~") gt
                    in lt

null :: ConfigPlan (CritBit Text a) -> Bool
null = foldPlan True (&&) ((CB.null .) . submap) T.empty

subgroups :: Text -> ConfigMap a -> [Text]
subgroups = loop
  where
    stripPrefixes pre = mapMaybe (stripPrefix pre)
    addPrefixes   pre = map (addPrefix pre)

    loop !_key Empty = []
    loop !key (Subconfig   pre pl) =
        stripPrefixes pre (loop (addPrefix pre key) pl)
    loop !key (Group pre pl) =
        case T.commonPrefixes pre key of
          Nothing
              | T.null pre -> loop key pl
              | T.null key ->
                  if null pl
                  then []
                  else [T.takeWhile ('.' /=) pre]
              | otherwise  -> []
          Just (prefix, pre', key')
              | T.null key' ->
                  if T.null pre'
                  then addPrefixes pre (loop key' pl)
                  else if T.head pre' /= '.' || null pl
                       then []
                       else [addPrefix prefix
                                       (T.takeWhile ('.' /=) (T.tail pre'))]
              | T.null pre' ->
                  case T.stripPrefix "." key' of
                    Nothing -> []
                    Just key'' -> addPrefixes prefix (loop key'' pl)
              | otherwise   -> []
    loop !key (Union  pl1 pl2) =
        OL.unionBy compare (loop key pl2) (loop key pl1)
    loop !key (ConfigPlan map) = subgroupsMap key map

subgroupsMap :: Text -> CritBit Text a -> [Text]
subgroupsMap pre_ map = loop (CB.lookupGT pre map)
  where
    pre | T.null pre_ = T.empty
        | otherwise   = pre_ <> "."
    loop Nothing = []
    loop (Just (key,_)) =
        case T.stripPrefix pre key of
          Nothing -> []
          Just sfx -> let (sfxa, sfxz) = T.break ('.' ==) sfx
                       in if T.null sfxz
                          then loop (CB.lookupGT key map)
                          else let key' = pre <> sfxa
                                in key' : loop (CB.lookupGE (key' <> "/") map)


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
--- There are at least three obvious "implementations" of <|> on ConfigParser
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
