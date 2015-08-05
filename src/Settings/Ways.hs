module Settings.Ways (
    ways, getWays,
    rtsWays, getRtsWays,
    knownWays, knownRtsWays
    ) where

import Way
import Stage
import Switches
import Expression
import Oracles.Flag
import Settings.User hiding (parallel)

-- Combining default ways with user modifications
ways :: Ways
ways = defaultWays <> userWays

rtsWays :: Ways
rtsWays = defaultRtsWays <> userRtsWays

getWays :: Expr [Way]
getWays = fromDiffExpr ways

getRtsWays :: Expr [Way]
getRtsWays = fromDiffExpr rtsWays

-- These are default ways
defaultWays :: Ways
defaultWays = mconcat
    [                              append [vanilla] -- always build vanilla
    , notStage Stage0            ? append [profiling]
    , platformSupportsSharedLibs ? append [dynamic] ]

defaultRtsWays :: Ways
defaultRtsWays = do
    ways <- getWays
    mconcat
        [ append [ logging, debug, threaded, threadedDebug, threadedLogging ]
        , (profiling `elem` ways) ? append [threadedProfiling]
        , (dynamic `elem` ways) ?
          append [ dynamic, debugDynamic, threadedDynamic, threadedDebugDynamic
                 , loggingDynamic, threadedLoggingDynamic ] ]

-- These are all ways known to the build system
knownWays :: [Way]
knownWays = [vanilla, profiling, logging, parallel, granSim]

knownRtsWays :: [Way]
knownRtsWays = [ threaded, threadedProfiling, threadedLogging, debug
               , debugProfiling, threadedDebug, threadedDebugProfiling, dynamic
               , profilingDynamic, threadedProfilingDynamic, threadedDynamic
               , threadedDebugDynamic, debugDynamic, loggingDynamic
               , threadedLoggingDynamic ]
