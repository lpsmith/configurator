{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Prelude hiding (lookup)

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Configurator
import           Data.Configurator.Parser
import           Data.Configurator.Types
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Data.Text (Text)
import           Data.Word
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testCase "load" loadTest
    , testCase "types" typesTest
    , testCase "interp" interpTest
    , testCase "scoped-interp" scopedInterpTest
    , testCase "import" importTest
--    , testCase "reload" reloadTest
    ]

withLoad :: FilePath -> (ConfigCache -> IO ()) -> IO ()
withLoad name t = do
    mb <- try $ load (testFile name)
    case mb of
        Left (err :: SomeException) -> assertFailure (show err)
        Right cfg -> t cfg

withReload :: FilePath -> ([Maybe FilePath] -> ConfigCache -> IO ()) -> IO ()
withReload name t = do
    tmp   <- getTemporaryDirectory
    temps <- forM (testFile name) $ \f -> do
        exists <- doesFileExist (worth f)
        if exists
            then do
                (p,h) <- openBinaryTempFile tmp "test.cfg"
                L.hPut h =<< L.readFile (worth f)
                hClose h
                return (p <$ f, Just p)
            else do
                return (f, Nothing)
    flip finally (mapM_ removeFile (catMaybes (map snd temps))) $ do
        mb <- try $ autoReload autoConfig (map fst temps)
        case mb of
            Left (err :: SomeException) -> assertFailure (show err)
            Right (cfg, tid) -> t (map snd temps) cfg >> killThread tid

testFile :: FilePath -> [Worth FilePath]
testFile name = [Required $ "tests" </> "resources" </> name]

takeMVarTimeout :: Int -> MVar a -> IO (Maybe a)
takeMVarTimeout millis v = do
    w <- newEmptyMVar
    tid <- forkIO $ do
        putMVar w . Just =<< takeMVar v
    forkIO $ do
        threadDelay (millis * 1000)
        killThread tid
        tryPutMVar w Nothing
        return ()
    takeMVar w

loadTest :: Assertion
loadTest =
  withLoad "pathological.cfg" $ \cfgcache -> do
    cfg <- readConfig cfgcache

    let (aa, _errs) = runParserM (key "aa") cfg
    assertEqual "int property" aa $ (Just 1 :: Maybe Int)

    let (ab, _errs) = runParserM (key "ab") cfg
    assertEqual "string property" ab (Just "foo" :: Maybe Text)

    let (acx, _errs) = runParserM (key "ac.x") cfg
    assertEqual "nested int" acx (Just 1 :: Maybe Int)

    let (acy, _errs) = runParserM (key "ac.y") cfg
    assertEqual "nested bool" acy (Just True :: Maybe Bool)

    let (ad, _errs) = runParserM (key "ad") cfg
    assertEqual "simple bool" ad (Just False :: Maybe Bool)

    let (ae, _errs) = runParserM (key "ae") cfg
    assertEqual "simple int 2" ae (Just 1 :: Maybe Int)

    let (af, _errs) = runParserM (key "af") cfg
    assertEqual "list property" af (Just (2,3) :: Maybe (Int,Int))

    let (deep, _errs) = runParserM (key "ag.q-e.i_u9.a") cfg
    assertEqual "deep bool" deep (Just False :: Maybe Bool)

    let (notacomment, _errs) = runParserM (key "notacomment") cfg
    assertEqual "not a comment" notacomment (Just 42 :: Maybe Int)

    let (comment, _errs) = runParserM (key "comment.x") cfg
    assertEqual "comment" comment (Nothing :: Maybe Value)

typesTest :: Assertion
typesTest =
  withLoad "pathological.cfg" $ \cfgcache -> do
    cfg <- readConfig cfgcache

    let (asInt, _errs) = runParserM (key "aa" :: ConfigParserM Int) cfg
    assertEqual "int" asInt (Just 1)

    let (asInteger, _errs) = runParserM (key "aa" :: ConfigParserM Integer) cfg
    assertEqual "int" asInteger (Just 1)

    let (asWord, _errs) = runParserM (key "aa" :: ConfigParserM Word) cfg
    assertEqual "int" asWord (Just 1)

    let (asInt8, _errs) = runParserM (key "aa" :: ConfigParserM Int8) cfg
    assertEqual "int8" asInt8 (Just 1)

    let (asInt16, _errs) = runParserM (key "aa" :: ConfigParserM Int16) cfg
    assertEqual "int16" asInt16 (Just 1)

    let (asInt32, _errs) = runParserM (key "aa" :: ConfigParserM Int32) cfg
    assertEqual "int32" asInt32 (Just 1)

    let (asInt64, _errs) = runParserM (key "aa" :: ConfigParserM Int64) cfg
    assertEqual "int64" asInt64 (Just 1)

    let (asWord8, _errs) = runParserM (key "aa" :: ConfigParserM Word8) cfg
    assertEqual "word8" asWord8 (Just 1)

    let (asWord16, _errs) = runParserM (key "aa" :: ConfigParserM Word16) cfg
    assertEqual "word16" asWord16 (Just 1)

    let (asWord32, _errs) = runParserM (key "aa" :: ConfigParserM Word32) cfg
    assertEqual "word32" asWord32 (Just 1)

    let (asWord64, _errs) = runParserM (key "aa" :: ConfigParserM Word64) cfg
    assertEqual "word64" asWord64 (Just 1)

    let (asTextBad, _errs) = runParserM (key "aa" :: ConfigParserM Text) cfg
    assertEqual "bad text" asTextBad Nothing

    let (asTextGood, _errs) = runParserM (key "ab" :: ConfigParserM Text) cfg
    assertEqual "good text" asTextGood (Just "foo")

    let (asStringGood, _errs) = runParserM (key "ab" :: ConfigParserM String) cfg
    assertEqual "string" asStringGood (Just "foo")

    let (asInts, _errs) = runParserM (key "xs" :: ConfigParserM [Int]) cfg
    assertEqual "ints" asInts (Just [1,2,3])

    let (asChar, _errs) = runParserM (key "c" :: ConfigParserM Char) cfg
    assertEqual "char" asChar (Just 'x')

interpTest :: Assertion
interpTest =
  withLoad "pathological.cfg" $ \cfgcache -> do
    cfg <- readConfig cfgcache

    home    <- getEnv "HOME"
    let (cfgHome, _errs) = runParserM (key "ba") cfg
    assertEqual "home interp" (Just home) cfgHome

scopedInterpTest :: Assertion
scopedInterpTest = withLoad "interp.cfg" $ \cfgcache -> do
    cfg <- readConfig cfgcache
    home    <- getEnv "HOME"

    let (a, _err) = runParserM (key "myprogram.exec") cfg
    assertEqual "myprogram.exec" (Just $ home++"/services/myprogram/myprogram") a

    let (b, _err) = runParserM (key "myprogram.stdout") cfg
    assertEqual "myprogram.stdout" (Just $ home++"/services/myprogram/stdout") b

    let (c, _err) = runParserM (key "top.layer1.layer2.dir") cfg
    assertEqual "nested scope" (Just $ home++"/top/layer1/layer2") c

importTest :: Assertion
importTest =
  withLoad "import.cfg" $ \cfgcache -> do
    cfg <- readConfig cfgcache
    let (aa, _errs) = runParserM (key "x.aa" :: ConfigParserM Int) cfg
    assertEqual "simple" aa (Just 1)
    let (acx, _errs) = runParserM (key "x.ac.x" :: ConfigParserM Int) cfg
    assertEqual "nested" acx (Just 1)

{--
reloadTest :: Assertion
reloadTest =
  withReload "pathological.cfg" $ \[Just f] cfgcache -> do
    aa <- lookup cfg "aa"
    assertEqual "simple property 1" aa $ Just (1 :: Int)

    dongly <- newEmptyMVar
    wongly <- newEmptyMVar
    subscribe cfg "dongly" $ \ _ _ -> putMVar dongly ()
    subscribe cfg "wongly" $ \ _ _ -> putMVar wongly ()
    L.appendFile f "\ndongly = 1"
    r1 <- takeMVarTimeout 2000 dongly
    assertEqual "notify happened" r1 (Just ())
    r2 <- takeMVarTimeout 2000 wongly
    assertEqual "notify not happened" r2 Nothing
--}
