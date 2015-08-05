module Settings.User (
    userArgs, userPackages, userWays, userRtsWays, userTargetDirectory,
    userKnownPackages, integerLibrary,
    buildHaddock, validating, ghciWithDebugger, ghcProfiled,
    dynamicGhcPrograms, laxDependencies
    ) where

import Stage
import Package
import Expression
import Settings.Default

-- No user-specific settings by default
-- TODO: rename to userArgs
userArgs :: Args
userArgs = mempty

-- Control which packages get to be built
userPackages :: Packages
userPackages = mempty

-- Add new user-defined packages
userKnownPackages :: [Package]
userKnownPackages = []

-- Control which ways are built
userWays :: Ways
userWays = mempty

userRtsWays :: Ways
userRtsWays = mempty

-- Control where build results go (see Settings.Default for an example)
userTargetDirectory :: Stage -> Package -> FilePath
userTargetDirectory = defaultTargetDirectory

-- Choose integer library: integerGmp, integerGmp2 or integerSimple
integerLibrary :: Package
integerLibrary = integerGmp2

-- User-defined flags. Note the following type semantics:
-- * Bool: a plain Boolean flag whose value is known at compile time
-- * Action Bool: a flag whose value can depend on the build environment
-- * Predicate: a flag depending on the build environment and the current target
validating :: Bool
validating = False

dynamicGhcPrograms :: Bool
dynamicGhcPrograms = False

ghciWithDebugger :: Bool
ghciWithDebugger = False

ghcProfiled :: Bool
ghcProfiled = False

laxDependencies :: Bool
laxDependencies = False

buildHaddock :: Predicate
buildHaddock = return True

