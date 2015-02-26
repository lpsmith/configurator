{-# LANGUAGE OverloadedStrings, ScopedTypeVariables    #-}
{-# LANGUAGE BangPatterns, ViewPatterns, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable         #-}

module Data.Configurator.ConfigMap where

import           Prelude hiding ((++),null)
import           Control.Applicative
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

newtype RMW r w a = RMW { runRMW :: r -> (Maybe a, w) }
    deriving (Typeable, Functor)

instance (Monoid w) => Applicative (RMW r w) where
    pure x  = RMW $ \_ -> (pure x, mempty)
    f <*> a = RMW $ \c -> let (mf, w ) = runRMW f c
                              (ma, w') = runRMW a c
                           in (mf <*> ma, w <> w')

-- |  The purpose of this function is to make it convenient to declare
--    a 'Monad' instance for 'ConfigParser' in order to use do-notation.
--    Be warned that this is an abuse.   A safe way to use this
--    function would be to treat is as applicative-do notation.  A safer
--    alternative would be to use the @applicative-quoters@ package
--    and not use this function at all.

unsafeBind :: Monoid w => RMW r w a -> (a -> RMW r w b) -> RMW r w b
unsafeBind m k =
    RMW $ \r ->
        case runRMW m r of
          (Nothing, w) -> let (_, w') = runRMW (k undefined) r
                           in (Nothing, w <> w')
          (Just a,  w) -> let (mb, w') = runRMW (k a) r
                           in (mb, w <> w')

safeBind :: Monoid w => RMW r w a -> (a -> RMW r w b) -> RMW r w b
safeBind m k =
    RMW $ \r ->
        let (ma, w ) = runRMW m r
            (mb, w') = case ma of
                         Nothing -> (Nothing, mempty)
                         Just a  -> runRMW (k a) r
         in (mb, w <> w')

{--
newtype RMWM r w a = RMWM { runRMWM :: RMW r w a } deriving (Typeable, Functor)

instance Monoid w => Applicative (RMWM r w) where
    pure x = RMWM (pure x)
    (<*>)  = ap

instance Monoid w => Monad (RMWM r w) where
    return = pure
    m >>= f = RMWM ( runRMWM m >>= \a -> runRMWM (f a) )
--}

type ConfigParser  =  RMW (ConfigMap Value) (DList ConfigParseError)

-- type ConfigParserM = RMWM (ConfigMap Value) (DList ConfigError)

withRMW :: Monoid w
        => (r -> r') -> RMW r' w' a -> RMW r w (Maybe a, w')
withRMW f m = RMW $ \r -> (Just (runRMW m (f r)), mempty)

{--
class ConfigP m where
  withConfig :: (ConfigMap Value -> ConfigMap Value)
             -> m a -> m (Maybe a, DList ConfigError)
  required :: Configured a => Name -> m a
  optional :: Configured a => Name -> a -> m a
  optionalPred :: Configured a => Name -> a -> (a -> Bool) -> m a

instance ConfigP ConfigParser where
  withConfig = withRMW
  required name
      = RMW $ \c ->
          case CM.lookup name config of
--}

withConfig :: (ConfigMap Value -> ConfigMap Value)
           -> ConfigParser a -> ConfigParser (Maybe a, DList ConfigParseError)
withConfig = withRMW


getLookupPlan :: Name -> ConfigPlan a -> DList Name
getLookupPlan = foldPlan DL.empty (<>) (\k _ -> DL.singleton k)

required :: (Configured a, Typeable a) => Name -> ConfigParser a
required name = requiredPred name (const True)

requiredPred :: forall a. (Configured a, Typeable a)
             => Name -> (a -> Bool) -> ConfigParser a
requiredPred name pred =
    RMW $ \c ->
        case lookupWithName name c of
          Nothing ->
              let err = ConfigParseError {
                           configErrorKeys = DL.toList (getLookupPlan name c),
                           configErrorVal  = Nothing,
                           configErrorType = typeOf (undefined :: a),
                           configErrorDef  = Nothing,
                           configErrorWhy  = Missing
                        }
               in (Nothing, DL.singleton err)
          Just (name', v) ->
              case convert v of
                Nothing ->
                    let err = ConfigParseError {
                           configErrorKeys = [name'],
                           configErrorVal  = Just v,
                           configErrorType = typeOf (undefined :: a),
                           configErrorDef  = Nothing,
                           configErrorWhy  = ConversionError
                         }
                     in (Nothing, DL.singleton err)
                Just v' ->
                    let err = ConfigParseError {
                           configErrorKeys = [name'],
                           configErrorVal  = Just v,
                           configErrorType = typeOf (undefined :: a),
                           configErrorDef  = Nothing,
                           configErrorWhy  = PredicateFailed
                        }
                     in if pred v'
                        then (Just v', mempty)
                        else (Nothing, DL.singleton err)


optional :: forall a. (Configured a, Typeable a, Show a)
         => Name -> a -> ConfigParser a
optional name def = optionalPred name def (const True)


optionalPred :: forall a. (Configured a, Typeable a, Show a)
             => Name -> a -> (a -> Bool) -> ConfigParser a
optionalPred name def pred =
    RMW $ \c ->
        case lookupWithName name c of
          Nothing ->
              let err = ConfigParseError {
                           configErrorKeys = DL.toList (getLookupPlan name c),
                           configErrorVal  = Nothing,
                           configErrorType = typeOf (undefined :: a),
                           configErrorDef  = Just (show def),
                           configErrorWhy  = Missing
                        }
               in (Just def, DL.singleton err)
          Just (name', v) ->
              case convert v of
                Nothing ->
                    let err = ConfigParseError {
                           configErrorKeys = [name'],
                           configErrorVal  = Just v,
                           configErrorType = typeOf (undefined :: a),
                           configErrorDef  = Just (show def),
                           configErrorWhy  = ConversionError
                         }
                     in (Just def, DL.singleton err)
                Just v' ->
                    let err = ConfigParseError {
                           configErrorKeys = [name'],
                           configErrorVal  = Just v,
                           configErrorType = typeOf (undefined :: a),
                           configErrorDef  = Just (show def),
                           configErrorWhy  = PredicateFailed
                        }
                     in if pred v'
                        then (Just v', mempty)
                        else (Just def, DL.singleton err)

{--






instance Applicative ConfigParser where
    pure x = ConfigParser $ \_ -> (Just x,mempty)
    f <*> a = ConfigParser $ \c -> let !(mf,errs ) = runParser f c
                                        (ma,errs') = runParser a c
                                    in  (mf <*> ma, errs <> errs')



unsafeBind :: ConfigParser a -> (a -> ConfigParser b) -> ConfigParser b
unsafeBind m f =
    ConfigParser $ \c ->
        case runParser m c of
          (Nothing, errs) -> let (_, errs') = runParser (f errmsg)
                              in (Nothing, errs ++ errs')
          (Just a,  errs) -> let (mb,errs') = runParser (f a)
                              in (mb, errs ++ errs')
  where
    errmsg = error "ConfigParser internal error"


safeBind :: ConfigParser a -> (a -> ConfigParser b) -> ConfigParser b
safeBind m f =
    ConfigParser $ \c ->
        let (a, errs ) = runParser m c
            (b, errs') = runParser (f a) c
         in case runParser m c of
              (Nothing, errs) -> (Nothing, errs)
              (Just a,  errs) -> let (mb,errs') = runParser (f a)
                                  in (mb, errs <> errs')

newtype ConfigParserM a
    = ConfigParserM {
        runConfigParserM :: ConfigParser a
      } deriving (Typeable, Functor)

instance Applicative ConfigParserM where
    pure a = ConfigParserM (pure a)
    (<*>) = ap

instance Monad ConfigParserM where
    return = pure
    m >>= f = ConfigParserM (runConfigParserM m `safeBind` \a ->
                                 runConfigParserM (f a))


--}
