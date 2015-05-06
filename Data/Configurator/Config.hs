{-# LANGUAGE OverloadedStrings, ScopedTypeVariables    #-}
{-# LANGUAGE BangPatterns, ViewPatterns, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable         #-}

module Data.Configurator.Config where

import           Prelude hiding ((++),null)
import           Control.Applicative
-- import           Control.Arrow(first)
import           Data.Maybe(mapMaybe)
--import           Data.Ratio
--import           Data.ByteString (ByteString)
import           Data.Configurator.Types.Internal hiding (Group)
import           Data.Typeable
import           Data.CritBit.Map.Lazy (CritBit)
import qualified Data.CritBit.Map.Lazy as CB
import qualified Data.List.Ordered as OL
import           Data.Monoid
import           Data.Function (on)
import           Data.Text(Text)
import qualified Data.Text as T
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
