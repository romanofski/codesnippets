#!/usr/bin/env stack
-- stack --resolver lts-12.5 script --ghc-options "-threaded" --package async,stm,bytestring,typed-process,process,temporary
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
import System.Process.Typed
import qualified System.Process as P
import System.IO (stdout, stderr)
import GHC.IO.Handle (hGetContents, hGetLine, hClose, Handle)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, throwIO)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically, STM, throwSTM, readTMVar, tryPutTMVar
                              , newEmptyTMVarIO, putTMVar)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.ByteString as B
import System.Exit (ExitCode(..))
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempFile)

main :: IO ()
main = do
  argv <- getArgs
  -- !!! no args -> crash !!
  withSystemTempFile "interleaved-output" $ \fp h -> do
    B.hPut h "\nfor i in {1..4064}; do\necho 'stdout';\n>&2 echo 'stderr';\necho 'stdout';\ndone"
    hClose h

    let config = proc "bash" [fp]

    (exitc, out) <- readProcessInterleaved config
    print out

readProcessInterleaved
  :: ProcessConfig stdinClosed stdout stderr
  -> IO (ExitCode, L.ByteString)
readProcessInterleaved pc = do
  mvar <- newEmptyTMVarIO
  (readEnd, writeEnd) <- P.createPipe
  t <- async $ do
        let loop front = do
                bs <- B.hGetSome readEnd defaultChunkSize
                if B.null bs
                    then atomically $ putTMVar mvar $ Right $ L.fromChunks $ front []
                    else loop $ front . (bs:)
            pc = proc "" []
        loop id `catch` \e -> do
            atomically $ void $ tryPutTMVar mvar $ Left $ ByteStringOutputException e pc
            throwIO e
        pure (readTMVar mvar >>= either throwSTM return, hClose readEnd)
  exit <- withProcess
    (setStdin closed $
     setStderr (useHandleOpen writeEnd) $
     setStdout (useHandleOpen writeEnd) pc) (atomically . waitExitCodeSTM)
  out <- wait t >>= atomically . fst
  pure (exit, out)

byteStringOutput' :: Handle -> StreamSpec 'STOutput (STM L.ByteString)
byteStringOutput' h = mkStreamSpec (P.UseHandle h) $ \pc Nothing -> do
    mvar <- newEmptyTMVarIO

    void $ async $ do
        let loop front = do
                bs <- B.hGetSome h defaultChunkSize
                if B.null bs
                    then atomically $ putTMVar mvar $ Right $ L.fromChunks $ front []
                    else loop $ front . (bs:)
        loop id `catch` \e -> do
            atomically $ void $ tryPutTMVar mvar $ Left $ ByteStringOutputException e pc
            throwIO e

    return (readTMVar mvar >>= either throwSTM return, hClose h)
