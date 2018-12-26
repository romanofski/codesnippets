#!/usr/bin/env stack
-- stack --resolver lts-12.5 script --package async --package stm --package bytestring --package typed-process --package process --package temporary
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
import System.Process.Typed
import qualified System.Process as P
import System.IO (stdout, stderr)
import GHC.IO.Handle (hGetContents, hGetLine, hClose, Handle)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, throwIO)
import Control.Concurrent.Async (async)
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
    B.hPut h "\necho 'stdout'\n>&2 echo 'stderr'\necho 'stdout'"
    hClose h

    let config = proc "bash" [fp]

    print "Capturing"
    (exitc, out) <- readProcessInterleaved config
    print out

readProcessInterleaved
  :: ProcessConfig stdinClosed stdout stderr
  -> IO (ExitCode, String)
readProcessInterleaved pc = do
    (readEnd, writeEnd) <- P.createPipe
    exitc' <- withProcess (setStderr (useHandleOpen writeEnd) $ setStdout (useHandleOpen writeEnd) pc') $ \p -> atomically $ do
       exitc <- waitExitCodeSTM p
       pure exitc
    hClose writeEnd
    c <- hGetContents readEnd
    pure (exitc', c)
  where
    pc' = setStdin closed pc

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
