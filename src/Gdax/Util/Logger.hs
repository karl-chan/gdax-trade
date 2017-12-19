{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Gdax.Util.Logger where

import Gdax.Util.Config.Log

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import Data.String.Conversions
import qualified Data.Traversable as T
import qualified GHC.Stack as GHC
import System.IO.Unsafe
import System.Log.FastLogger

data Loggers = Loggers
  { fileLogger :: !(Maybe (FastLogger, IO ()))
  , stderrLogger :: !(Maybe (FastLogger, IO ()))
  }

-- | Log with 'LogTrace' log level
logTrace ::
     (?callStack :: GHC.CallStack)
  => MonadIO m =>
       String -> m ()
logTrace = doLog LogTrace ?callStack

-- | Log with 'LogDebug' log level
logDebug ::
     (?callStack :: GHC.CallStack)
  => MonadIO m =>
       String -> m ()
logDebug = doLog LogDebug ?callStack

-- | Log with 'LogInfo' log level
logInfo ::
     (?callStack :: GHC.CallStack)
  => MonadIO m =>
       String -> m ()
logInfo = doLog LogInfo ?callStack

-- | Log with 'LogNote' log level
logNote ::
     (?callStack :: GHC.CallStack)
  => MonadIO m =>
       String -> m ()
logNote = doLog LogNote ?callStack

-- | Log with 'LogWarn' log level
logWarn ::
     (?callStack :: GHC.CallStack)
  => MonadIO m =>
       String -> m ()
logWarn = doLog LogWarn ?callStack

-- | Log with 'LogError' log level
logError ::
     (?callStack :: GHC.CallStack)
  => MonadIO m =>
       String -> m ()
logError = doLog LogError ?callStack

-- | Log on error level and call 'fail'
logFail ::
     (?callStack :: GHC.CallStack)
  => MonadIO m =>
       String -> m a
logFail t = do
  doLog LogError ?callStack t
  fail t

-- | Log with 'LogTrace' level when the given expression is evaluated
pureTrace :: (?callStack :: GHC.CallStack) => String -> a -> a
pureTrace = doPureLog LogTrace ?callStack

-- | Log with 'LogDebug' level when the given expression is evaluated
pureDebug :: (?callStack :: GHC.CallStack) => String -> a -> a
pureDebug = doPureLog LogDebug ?callStack

-- | Log with 'LogInfo' level when the given expression is evaluated
pureInfo :: (?callStack :: GHC.CallStack) => String -> a -> a
pureInfo = doPureLog LogInfo ?callStack

-- | Log with 'LogNote' level when the given expression is evaluated
pureNote :: (?callStack :: GHC.CallStack) => String -> a -> a
pureNote = doPureLog LogNote ?callStack

-- | Log with 'LogWarn' level when the given expression is evaluated
pureWarn :: (?callStack :: GHC.CallStack) => String -> a -> a
pureWarn = doPureLog LogWarn ?callStack

-- | Log with 'LogError' level when the given expression is evaluated
pureError :: (?callStack :: GHC.CallStack) => String -> a -> a
pureError = doPureLog LogError ?callStack

doPureLog :: LogLevel -> GHC.CallStack -> String -> a -> a
doPureLog ll cs txt expr = unsafePerformIO (doLog ll cs txt) `seq` expr

doLog :: MonadIO m => LogLevel -> GHC.CallStack -> String -> m ()
doLog ll cs txt =
  liftIO $
  readIORef logLevelRef >>= \logLim ->
    when (ll >= logLim) $ do
      Loggers {..} <- readIORef loggersRef
      let loc =
            case GHC.getCallStack cs of
              ((_, l):_) ->
                GHC.srcLocFile l <> ":" <> show (GHC.srcLocStartLine l)
              _ -> "unknown"
          msg =
            "[" <> renderLevel <> "] " <> toLogStr loc <> " - " <> toLogStr txt <>
            "\n"
      forM_ stderrLogger $ \(writeLog, _) -> writeLog msg
      forM_ fileLogger $ \(writeLog, _) -> writeLog msg
  where
    renderLevel =
      case ll of
        LogTrace -> "TRACE"
        LogDebug -> "DEBUG"
        LogInfo -> "INFO"
        LogNote -> "NOTE"
        LogWarn -> "WARN"
        LogError -> "ERROR"

loggersRef :: IORef Loggers
loggersRef = unsafePerformIO $ newIORef (Loggers Nothing Nothing)

{-# NOINLINE loggersRef #-}
logLevelRef :: IORef LogLevel
logLevelRef = unsafePerformIO $ newIORef LogDebug

{-# NOINLINE logLevelRef #-}
-- | Set the verbosity level. Messages at our higher than this level are
-- displayed.  It defaults to 'LogDebug'.
setLogLevel :: LogLevel -> IO ()
setLogLevel = atomicWriteIORef logLevelRef

-- | Setup global logging. Wrap your 'main' function with this.
withGlobalLogging :: LogConf -> IO a -> IO a
withGlobalLogging LogConf {..} f = bracket initLogger flushLogger (const f)
  where
    flushLogger (Loggers a b) = do
      forM_ a $ \(_, flush) -> flush
      forM_ b $ \(_, flush) -> flush
    initLogger = do
      fileLogger <-
        flip T.mapM logFile $ \fp -> do
          let spec =
                FileLogSpec
                { log_file = fp
                , log_file_size = 1024 * 1024 * 50
                , log_backup_number = 5
                }
          newFastLogger (LogFile spec defaultBufSize)
      stderrLogger <-
        if enableStderr
          then Just <$> newFastLogger (LogStderr defaultBufSize)
          else pure Nothing
      let lgrs = Loggers fileLogger stderrLogger
      writeIORef loggersRef lgrs
      setLogLevel logLevel
      pure lgrs
