{-# LANGUAGE FlexibleInstances, PatternGuards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Configurator.Instances () where

import Control.Applicative
import Data.Configurator.Types.Internal
import Data.Complex (Complex((:+)))
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text.Encoding (encodeUtf8)
import Data.Ratio (Ratio, (%))
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.C.Types (CDouble, CFloat)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Scientific ( Scientific,  coefficient, base10Exponent
                       , floatingOrInteger, toRealFloat, toBoundedInteger )

instance Configured Value where
    convert = id

instance Configured Bool where
    convert (Just (Bool v)) = Just v
    convert _               = Nothing

convertNumberToBoundedInteger :: (Integral a, Bounded a) => Maybe Value -> Maybe a
convertNumberToBoundedInteger (Just (Number r)) = toBoundedInteger r
convertNumberToBoundedInteger _ = Nothing

convertNumberToInteger :: forall a. (Integral a) => Maybe Value -> Maybe a
convertNumberToInteger (Just (Number r))
    | Right n <- floatingOrInteger r :: Either Float a = Just n
convertNumberToInteger _ = Nothing

instance Configured Int where
    convert = convertNumberToBoundedInteger

instance Configured Integer where
    convert = convertNumberToInteger

instance Configured Int8 where
    convert = convertNumberToBoundedInteger

instance Configured Int16 where
    convert = convertNumberToBoundedInteger

instance Configured Int32 where
    convert = convertNumberToBoundedInteger

instance Configured Int64 where
    convert = convertNumberToBoundedInteger

instance Configured Word where
    convert = convertNumberToBoundedInteger

instance Configured Word8 where
    convert = convertNumberToBoundedInteger

instance Configured Word16 where
    convert = convertNumberToBoundedInteger

instance Configured Word32 where
    convert = convertNumberToBoundedInteger

instance Configured Word64 where
    convert = convertNumberToBoundedInteger

convertNumberToRealFloat :: (RealFloat a) => Maybe Value -> Maybe a
convertNumberToRealFloat v =
    case v of
      (Just (Number r)) -> Just $ toRealFloat r
      _                 -> Nothing

convertNumberToFractional :: (Fractional a) => Maybe Value -> Maybe a
convertNumberToFractional (Just (Number r)) = Just $ fromRational r'
  where
    c  = coefficient    r
    e  = base10Exponent r
    r' = if e >= 0
         then (c * 10^e) % 1
         else c % (10^(- e))
convertNumberToFractional _ = Nothing

instance Configured Double where
    convert = convertNumberToRealFloat

instance Configured Float where
    convert = convertNumberToRealFloat

instance Configured CDouble where
    convert = convertNumberToRealFloat

instance Configured CFloat where
    convert = convertNumberToRealFloat

instance Integral a => Configured (Ratio a) where
    convert = convertNumberToFractional

instance Configured Scientific where
    convert (Just (Number r)) = Just r
    convert _                 = Nothing

instance RealFloat a => Configured (Complex a) where
    convert (Just (Number r)) = Just (toRealFloat r :+ 0)
    convert _                 = Nothing

instance HasResolution a => Configured (Fixed a) where
    convert = convertNumberToFractional

instance Configured T.Text where
    convert (Just (String v)) = Just v
    convert _                 = Nothing

instance Configured Char where
    convert (Just (String txt)) | T.length txt == 1 = Just $ T.head txt
    convert _ = Nothing

    convertList = fmap T.unpack . convert

instance Configured L.Text where
    convert = fmap L.fromStrict . convert

instance Configured B.ByteString where
    convert = fmap encodeUtf8 . convert

instance Configured LB.ByteString where
    convert = fmap (LB.fromChunks . (:[])) . convert

instance (Configured a, Configured b) => Configured (a,b) where
    convert (Just (List [a,b])) =
        (,) <$> convert (Just a) <*> convert (Just b)
    convert _ =
        Nothing

instance (Configured a, Configured b, Configured c) => Configured (a,b,c) where
    convert (Just (List [a,b,c])) =
        (,,) <$> convert (Just a) <*> convert (Just b) <*> convert (Just c)
    convert _ =
        Nothing

instance (Configured a, Configured b, Configured c, Configured d)
    => Configured (a,b,c,d) where
    convert (Just (List [a,b,c,d]))
        = (,,,) <$> convert (Just a) <*> convert (Just b) <*> convert (Just c)
                <*> convert (Just d)
    convert _
        = Nothing

instance (Configured a) => Configured (Maybe a) where
    convert Nothing  = Just Nothing
    convert (Just v) = Just <$> convert (Just v)
