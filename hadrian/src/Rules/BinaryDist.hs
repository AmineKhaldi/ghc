{-# LANGUAGE TupleSections #-}
module Rules.BinaryDist where

import Hadrian.Haskell.Cabal

import CommandLine
import Context
import Expression
import Oracles.Setting
import Oracles.Flag
import Packages
import Settings
import Settings.Program (programContext)
import Target
import Utilities
import qualified System.Directory.Extra as IO
import Data.Either

{-
Note [Binary distributions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Hadrian produces binary distributions under:
  <build root>/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>.tar.xz

It is generated by creating an archive from:
  <build root>/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/

It does so by following the steps below.

- make sure we have a complete stage 2 compiler + haddock

- copy the specific binaries which should be in the bindist to the
  bin folder and add the version suffix:
    <build root>/stage1/bin/xxxx
  to
    <build root/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/bin/xxxx-<VER>

- create symlink (or bash) wrapper from unversioned to versioned executable:
    <build root/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/bin/xxxx
  points to:
    <build root/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/bin/xxxx-<VER>

- copy the lib directories of the compiler we built:
    <build root>/stage1/lib
  to
    <build root>/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/lib

- copy the generated docs (user guide, haddocks, etc):
    <build root>/docs/
  to
    <build root>/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/docs/

- use autoreconf to generate a `configure` script from
  aclocal.m4 and distrib/configure.ac, that we move to:
    <build root>/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/configure

- write a (fixed) Makefile capable of supporting 'make install' to:
    <build root>/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/Makefile

- write some (fixed) supporting bash code for the wrapper scripts to:
    <build root>/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/wrappers/<program>

  where <program> is the name of the executable that the bash file will
  help wrapping.

- copy supporting configure/make related files
  (see @bindistInstallFiles@) to:
    <build root>/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/<file>

- create a .tar.xz archive of the directory:
    <build root>/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/
  at
    <build root>/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>.tar.xz


Note [Wrapper scripts and binary distributions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Users of Linux, FreeBSD, Windows and OS X can unpack a
binary distribution produced by hadrian for their arch
and OS and start using @bin/ghc@, @bin/ghc-pkg@ and so on
right away, without even having to configure or install
the distribution. They would then be using the real executables
directly, not through wrapper scripts.

This works because GHCs produced by hadrian on those systems
are relocatable. This means that you can copy the @bin@ and @lib@
dirs anywhere and GHC will keep working, as long as both
directories sit next to each other. (This is achieved by having
GHC look up its $libdir relatively to where the GHC executable
resides.)

It is however still possible (and simple) to install a GHC
distribution that uses wrapper scripts. From the unpacked archive,
you can simply do:

  ./configure --prefix=<path> [... other configure options ...]
  make install

In order to support @bin@ and @lib@ directories that don't sit next to each
other, the install script:
   * installs programs into @LIBDIR/ghc-VERSION/bin@
   * installs libraries into @LIBDIR/ghc-VERSION/lib@
   * installs the wrappers scripts into @BINDIR@ directory

-}

bindistRules :: Rules ()
bindistRules = do
    root <- buildRootRules
    phony "install" $ do
        need ["binary-dist-dir"]
        version        <- setting ProjectVersion
        targetPlatform <- setting TargetPlatformFull
        let ghcVersionPretty = "ghc-" ++ version ++ "-" ++ targetPlatform
            bindistFilesDir  = root -/- "bindist" -/- ghcVersionPretty
            prefixErr = "You must specify a path with --prefix when using the"
                     ++ " 'install' rule"
        installPrefix <- fromMaybe (error prefixErr) <$> cmdPrefix
        runBuilder (Configure bindistFilesDir) ["--prefix="++installPrefix] [] []
        runBuilder (Make bindistFilesDir) ["install"] [] []

    phony "binary-dist-dir" $ do
        -- We 'need' all binaries and libraries
        all_pkgs <- stagePackages Stage1
        (lib_targets, bin_targets) <- partitionEithers <$> mapM pkgTarget all_pkgs
        cross <- flag CrossCompiling
        iserv_targets <- if cross then pure [] else iservBins
        need (lib_targets ++ (map (\(_, p) -> p) (bin_targets ++ iserv_targets)))

        version        <- setting ProjectVersion
        targetPlatform <- setting TargetPlatformFull
        distDir        <- Context.distDir Stage1
        rtsDir         <- pkgIdentifier rts

        let ghcBuildDir      = root -/- stageString Stage1
            bindistFilesDir  = root -/- "bindist" -/- ghcVersionPretty
            ghcVersionPretty = "ghc-" ++ version ++ "-" ++ targetPlatform
            rtsIncludeDir    = ghcBuildDir -/- "lib" -/- distDir -/- rtsDir
                               -/- "include"

        -- We create the bindist directory at <root>/bindist/ghc-X.Y.Z-platform/
        -- and populate it with Stage2 build results
        createDirectory bindistFilesDir
        createDirectory (bindistFilesDir -/- "bin")
        createDirectory (bindistFilesDir -/- "lib")
        -- Also create wrappers with version suffixes (#20074)
        forM_ (bin_targets ++ iserv_targets) $ \(pkg, prog_path) -> do
            let orig_filename = takeFileName prog_path
                (name, ext) = splitExtensions orig_filename
                suffix = if useGhcPrefix pkg
                          then "ghc-" ++ version
                          else version
                version_prog = name ++ "-" ++ suffix ++ ext
                -- Install the actual executable with a version suffix
                install_path = bindistFilesDir -/- "bin" -/- version_prog
                -- The wrapper doesn't have a version
                unversioned_install_path = (bindistFilesDir -/- "bin" -/- orig_filename)
            -- 1. Copy the executable to the versioned executable name in
            -- the directory
            copyFile prog_path install_path
            -- 2. Either make a symlink for the unversioned version or
            -- a wrapper script on platforms (windows) which don't support symlinks.
            if windowsHost
              then createVersionWrapper pkg version_prog unversioned_install_path
              else liftIO $ do
                -- Use the IO versions rather than createFileLink because
                -- we need to create a relative symlink.
                IO.removeFile unversioned_install_path <|> return ()
                IO.createFileLink version_prog unversioned_install_path

            -- If we have runghc, also need runhaskell (#19571)
            -- Make links for both versioned and unversioned runhaskell to
            -- normal runghc
            when (pkg == runGhc) $ do
              let unversioned_runhaskell_path =
                    bindistFilesDir -/- "bin" -/- "runhaskell" ++ ext
                  versioned_runhaskell_path =
                    bindistFilesDir -/- "bin" -/- "runhaskell" ++ "-" ++ version ++ ext
              if windowsHost
                then do
                  createVersionWrapper pkg version_prog unversioned_runhaskell_path
                  createVersionWrapper pkg version_prog versioned_runhaskell_path
                else liftIO $ do
                  -- Unversioned
                  IO.removeFile unversioned_runhaskell_path <|> return ()
                  IO.createFileLink version_prog unversioned_runhaskell_path
                  -- Versioned
                  IO.removeFile versioned_runhaskell_path <|> return ()
                  IO.createFileLink version_prog versioned_runhaskell_path

        copyDirectory (ghcBuildDir -/- "lib") bindistFilesDir
        copyDirectory (rtsIncludeDir)         bindistFilesDir
        when windowsHost $ createGhcii (bindistFilesDir -/- "bin")

        -- Call ghc-pkg recache, after copying so the package.cache is
        -- accurate, then it's on the distributor to use `cp -a` to install
        -- a relocatable bindist.
        --
        -- N.B. the ghc-pkg executable may be prefixed with a target triple
        -- (c.f. #20267).
        ghcPkgName <- programName (vanillaContext Stage1 ghcPkg)
        cmd_ (bindistFilesDir -/- "bin" -/- ghcPkgName) ["recache"]


        -- The settings file must be regenerated by the bindist installation
        -- logic to account for the environment discovered by the bindist
        -- configure script on the host. Not on Windows, however, where
        -- we do not ship a configure script with the bindist. See #20254.
        --
        -- N.B. we must do this after ghc-pkg has been run as it will go
        -- looking for the settings files.
        unless windowsHost $
            removeFile (bindistFilesDir -/- "lib" -/- "settings")

        unless cross $ need ["docs"]

        -- TODO: we should only embed the docs that have been generated
        -- depending on the current settings (flavours' "ghcDocs" field and
        -- "--docs=.." command-line flag)
        -- Currently we embed the "docs" directory if it exists but it may
        -- contain outdated or even invalid data.

        -- Use the IO version of doesDirectoryExist because the Shake Action
        -- version should not be used for directories the build system can
        -- create. Using the Action version caused documentation to not be
        -- included in the bindist in the past (part of the problem in #18669).
        whenM (liftIO (IO.doesDirectoryExist (root -/- "doc"))) $ do
          copyDirectory (root -/- "doc") bindistFilesDir
          copyFile ("libraries" -/- "prologue.txt") (bindistFilesDir -/- "docs-utils" -/- "prologue.txt")
          copyFile ("libraries" -/- "gen_contents_index") (bindistFilesDir -/- "docs-utils" -/- "gen_contents_index" )

        when windowsHost $ do
          copyDirectory (root -/- "mingw") bindistFilesDir
          -- we use that opportunity to delete the .stamp file that we use
          -- as a proxy for the whole mingw toolchain, there's no point in
          -- shipping it
          removeFile (bindistFilesDir -/- mingwStamp)

        -- Include bash-completion script in binary distributions. We don't
        -- currently install this but merely include it for the user's
        -- reference. See #20802.
        copyDirectory ("utils" -/- "completion") bindistFilesDir

        -- These scripts are only necessary in the configure/install
        -- workflow which is not supported on windows.
        -- TODO: Instead of guarding against windows, we could offer the
        -- option to make a relocatable, but not installable bindist on any
        -- platform.
        unless windowsHost $ do
          -- We then 'need' all the files necessary to configure and install
          -- (as in, './configure [...] && make install') this build on some
          -- other machine.
          need $ map (bindistFilesDir -/-)
                    (["configure", "Makefile"] ++ bindistInstallFiles)
          copyFile ("hadrian" -/- "bindist" -/- "config.mk.in") (bindistFilesDir -/- "config.mk.in")
          forM_ bin_targets $ \(pkg, _) -> do
            needed_wrappers <- pkgToWrappers pkg
            forM_ needed_wrappers $ \wrapper_name -> do
              let suffix = if useGhcPrefix pkg
                             then "ghc-" ++ version
                             else version
              wrapper_content <- wrapper wrapper_name
              let unversioned_wrapper_path = bindistFilesDir -/- "wrappers" -/- wrapper_name
                  versioned_wrapper = wrapper_name ++ "-" ++ suffix
                  versioned_wrapper_path = bindistFilesDir -/- "wrappers" -/- versioned_wrapper
              -- Write the wrapper to the versioned path
              writeFile' versioned_wrapper_path wrapper_content
              -- Create a symlink from the non-versioned to the versioned.
              liftIO $ do
                IO.removeFile unversioned_wrapper_path <|> return ()
                IO.createFileLink versioned_wrapper unversioned_wrapper_path


    let buildBinDist :: Compressor -> Action ()
        buildBinDist compressor = do
            need ["binary-dist-dir"]

            version        <- setting ProjectVersion
            targetPlatform <- setting TargetPlatformFull

            let ghcVersionPretty = "ghc-" ++ version ++ "-" ++ targetPlatform

            -- Finally, we create the archive <root>/bindist/ghc-X.Y.Z-platform.tar.xz
            tarPath <- builderPath (Tar Create)
            cmd [Cwd $ root -/- "bindist"] tarPath
                [ "-c", compressorTarFlag compressor, "-f"
                , ghcVersionPretty <.> "tar" <.> compressorExtension compressor
                , ghcVersionPretty ]

    phony "binary-dist" $ buildBinDist Xz
    phony "binary-dist-gzip" $ buildBinDist Gzip
    phony "binary-dist-bzip2" $ buildBinDist Bzip2
    phony "binary-dist-xz" $ buildBinDist Xz

    -- Prepare binary distribution configure script
    -- (generated under <ghc root>/distrib/configure by 'autoreconf')
    root -/- "bindist" -/- "ghc-*" -/- "configure" %> \configurePath -> do
        ghcRoot <- topDirectory
        copyFile (ghcRoot -/- "aclocal.m4") (ghcRoot -/- "distrib" -/- "aclocal.m4")
        copyDirectory (ghcRoot -/- "m4") (ghcRoot -/- "distrib")
        buildWithCmdOptions [] $
            target (vanillaContext Stage1 ghc) (Autoreconf $ ghcRoot -/- "distrib") [] []
        -- We clean after ourselves, moving the configure script we generated in
        -- our bindist dir
        removeFile (ghcRoot -/- "distrib" -/- "aclocal.m4")
        removeDirectory (ghcRoot -/- "distrib" -/- "m4")

        moveFile (ghcRoot -/- "distrib" -/- "configure") configurePath

    -- Generate the Makefile that enables the "make install" part
    root -/- "bindist" -/- "ghc-*" -/- "Makefile" %> \makefilePath -> do
        top <- topDirectory
        copyFile (top -/- "hadrian" -/- "bindist" -/- "Makefile") makefilePath

    -- Copy various configure-related files needed for a working
    -- './configure [...] && make install' workflow
    -- (see the list of files needed in the 'binary-dist' rule above, before
    -- creating the archive).
    forM_ bindistInstallFiles $ \file ->
        root -/- "bindist" -/- "ghc-*" -/- file %> \dest -> do
            ghcRoot <- topDirectory
            copyFile (ghcRoot -/- fixup file) dest

  where
    fixup f | f `elem` ["INSTALL", "README"] = "distrib" -/- f
            | otherwise                      = f

data Compressor = Gzip | Bzip2 | Xz
                deriving (Eq, Ord, Show)

-- | Flag to pass to tar to use the given 'Compressor'.
compressorTarFlag :: Compressor -> String
compressorTarFlag Gzip  = "--gzip"
compressorTarFlag Xz    = "--xz"
compressorTarFlag Bzip2 = "--bzip"

-- | File extension to use for archives compressed with the given 'Compressor'.
compressorExtension :: Compressor -> String
compressorExtension Gzip  = "gz"
compressorExtension Xz    = "xz"
compressorExtension Bzip2 = "bz2"

-- | A list of files that allow us to support a simple
-- @./configure [...] && make install@ workflow.
bindistInstallFiles :: [FilePath]
bindistInstallFiles =
    [ "config.sub", "config.guess", "install-sh"
    , "mk" -/- "config.mk.in" -- TODO: Remove when make is gone
    , "mk" -/- "install.mk.in" -- TODO: Remove when make is gone
    , "mk" -/- "project.mk"
    , "mk" -/- "relpath.sh"
    , "mk" -/- "system-cxx-std-lib-1.0.conf.in"
    , "README", "INSTALL" ]

-- | This auxiliary function gives us a top-level 'Filepath' that we can 'need'
-- for all libraries and programs that are needed for a complete build.
-- For libraries, it returns the path to the @.conf@ file in the package
-- database. For programs, it returns the path to the compiled executable.
pkgTarget :: Package -> Action (Either FilePath (Package, FilePath))
pkgTarget pkg
    | isLibrary pkg = Left <$> pkgConfFile (vanillaContext Stage1 pkg)
    | otherwise     = do
        path <- programPath =<< programContext Stage1 pkg
        return (Right (pkg, path))

useGhcPrefix :: Package -> Bool
useGhcPrefix pkg
  | pkg == ghc    = False
  | pkg == runGhc = False
  | pkg == ghcPkg = False
  | pkg == ghciWrapper = False
  | otherwise = True


-- | Which wrappers point to a specific package
pkgToWrappers :: Package -> Action [String]
pkgToWrappers pkg
  -- ghc also has the ghci script wrapper
  | pkg == ghc = pure ["ghc", "ghci"]
  | pkg == runGhc = pure ["runghc", "runhaskell"]
  -- These are the packages which we want to expose to the user and hence
  -- there are wrappers installed in the bindist.
  | pkg `elem` [hpcBin, haddock, hp2ps, hsc2hs, ghc, ghcPkg]
    = (:[]) <$> (programName =<< programContext Stage1 pkg)
  | otherwise = pure []


wrapper :: FilePath -> Action String
wrapper "ghc"         = ghcWrapper
wrapper "ghc-pkg"     = ghcPkgWrapper
wrapper "ghci" = ghciScriptWrapper
wrapper "haddock"     = haddockWrapper
wrapper "hsc2hs"      = hsc2hsWrapper
wrapper "runghc"      = runGhcWrapper
wrapper "runhaskell"  = runGhcWrapper
wrapper _             = commonWrapper

-- | Wrapper scripts for different programs. Common is default wrapper.
-- See Note [Two Types of Wrappers]

ghcWrapper :: Action String
ghcWrapper = pure $ "exec \"$executablename\" -B\"$libdir\" ${1+\"$@\"}\n"

ghcPkgWrapper :: Action String
ghcPkgWrapper = pure $ unlines
    [ "PKGCONF=\"$libdir/package.conf.d\""
    , "exec \"$executablename\" --global-package-db \"$PKGCONF\" ${1+\"$@\"}" ]

haddockWrapper :: Action String
haddockWrapper = pure $ "exec \"$executablename\" -B\"$libdir\" -l\"$libdir\" ${1+\"$@\"}\n"

commonWrapper :: Action String
commonWrapper = pure $ "exec \"$executablename\" ${1+\"$@\"}\n"

-- echo 'HSC2HS_EXTRA="$(addprefix --cflag=,$(CONF_CC_OPTS_STAGE1)) $(addprefix --lflag=,$(CONF_GCC_LINKER_OPTS_STAGE1))"' >> "$(WRAPPER)"
hsc2hsWrapper :: Action String
hsc2hsWrapper = do
  ccArgs <- map ("--cflag=" <>) <$> settingList (ConfCcArgs Stage1)
  ldFlags <- map ("--lflag=" <>) <$> settingList (ConfGccLinkerArgs Stage1)
  wrapper <- drop 4 . lines <$> liftIO (readFile "utils/hsc2hs/hsc2hs.wrapper")
  return $ unlines
    ( "HSC2HS_EXTRA=\"" <> unwords (ccArgs ++ ldFlags) <> "\""
    : "tflag=\"--template=$libdir/template-hsc.h\""
    : "Iflag=\"-I$includedir/\""
    : wrapper )

runGhcWrapper :: Action String
runGhcWrapper = pure $ "exec \"$executablename\" -f \"$exedir/ghc\" ${1+\"$@\"}\n"

-- | We need to ship ghci executable, which basically just calls ghc with
-- | --interactive flag.
ghciScriptWrapper :: Action String
ghciScriptWrapper = do
  version <- setting ProjectVersion
  pure $ unlines
    [ "executable=\"$bindir/ghc-" ++ version ++ "\""
    , "exec $executable --interactive \"$@\"" ]

-- | When not on Windows, we want to ship the 3 flavours of the iserv program
--   in binary distributions. This isn't easily achievable by just asking for
--   the package to be built, since here we're generating 3 different
--   executables out of just one package, so we need to specify all 3 contexts
--   explicitly and 'need' the result of building them.
iservBins :: Action [(Package, FilePath)]
iservBins = do
  rtsways <- interpretInContext (vanillaContext Stage1 ghc) getRtsWays
  traverse (fmap (\p -> (iserv, p)) . programPath)
      [ Context Stage1 iserv w
      | w <- [vanilla, profiling, dynamic]
      , w `elem` rtsways
      ]

-- Version wrapper scripts
-- See Note [Two Types of Wrappers]

-- | Create a wrapper script calls the executable given as first argument
createVersionWrapper :: Package -> String -> FilePath -> Action ()
createVersionWrapper pkg versioned_exe install_path = do
  ghcPath <- builderPath (Ghc CompileCWithGhc Stage2)
  top <- topDirectory
  let version_wrapper_dir = top -/- "hadrian" -/- "bindist" -/- "cwrappers"
      wrapper_files = [ version_wrapper_dir -/- file | file <- ["version-wrapper.c", "getLocation.c", "cwrapper.c"]]
      -- If the wrapper is for an interactive process like GHCi then we need to call
      -- FreeConsole to pass event processing to the child process
      -- See #21889 and #14150 and #13411
      interactive
        | pkg == ghciWrapper = (1 :: Int)
        | otherwise = 0

  cmd ghcPath (["-no-hs-main", "-o", install_path, "-I"++version_wrapper_dir
              , "-DEXE_PATH=\"" ++ versioned_exe ++ "\""
              , "-DINTERACTIVE_PROCESS=" ++ show interactive
              ] ++ wrapper_files)

{-
Note [Two Types of Wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are two different types of wrapper scripts.

1. The wrapper scripts installed
    <build root>/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/wrappers/<program>
2. The version wrapper scripts installed in
    <build root/bindist/ghc-<X>.<Y>.<Z>-<arch>-<os>/bin/xxxx

The purpose of the wrappers in (1) is to allow the executables to be installed
into a different @BINDIR@ which is not already adjacent to a libdir. Therefore
these wrappers pass the libdir and so on explicitliy to the executable so the
wrappers can be placed anywhere and still work.

The purpose of the wrappers in (2) is to provide both versioned and unversioned
executables. On windows, these are actual wrapper scripts which just call the executable
but on linux these wrappers are symlinks.

-}

-- | On Windows ghci must be invoked via this wrapper script due to signal craziness.
-- When WinIO becomes the default it can be removed. See #19339 and #12720.
createGhcii :: FilePath -> Action ()
createGhcii outDir = do
    version <- setting ProjectVersion
    create (outDir -/- "ghcii.sh")
    create (outDir -/- "ghcii"++version++".sh")
  where
    create out = writeFileChanged out content >> makeExecutable out
    content = unlines
      [ "#!/bin/sh"
      , "exec \"$(dirname \"$0\")\"/ghc --interactive \"$@\""
      ]

