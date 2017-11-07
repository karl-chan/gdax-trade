module Gdax.Util.Config.Log where

import           Data.Char

data LogConfig = LogConfig
    { logFile      :: !(Maybe FilePath)
    , enableStderr :: !Bool
    , logLevel     :: LogLevel
    }

data LogLevel
    = LogTrace
    | LogDebug
    | LogInfo
    | LogNote
    | LogWarn
    | LogError
    deriving (Eq, Show, Read, Ord)

parseLogLevel :: String -> LogLevel
parseLogLevel s =
    case map toLower s of
        "trace" -> LogTrace
        "debug" -> LogDebug
        "info"  -> LogInfo
        "note"  -> LogNote
        "warn"  -> LogWarn
        "error" -> LogError
        _       -> LogDebug
