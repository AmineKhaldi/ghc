{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section{Code output phase}
-}

{-# LANGUAGE CPP #-}

module CodeOutput
   ( codeOutput
   , outputForeignStubs
   , ipInitCode
   )
where

#include "HsVersions.h"

import GhcPrelude

import AsmCodeGen       ( nativeCodeGen )
import LlvmCodeGen      ( llvmCodeGen )

import UniqSupply       ( mkSplitUniqSupply )

import Finder           ( mkStubPaths )
import PprC             ( writeC )
import CmmLint          ( cmmLint )
import Packages
import Cmm              ( RawCmmGroup, CmmInfoTable )
import HscTypes
import DynFlags
import Stream           ( Stream )
import qualified Stream
import FileCleanup

import ErrUtils
import Outputable
import Module
import SrcLoc
import IPE
import CostCentre
import Outputable
import CLabel
import GHC.Stg.Debug
import GHC.StgToCmm.Utils

import Control.Exception
import System.Directory
import System.FilePath
import System.IO

{-
************************************************************************
*                                                                      *
\subsection{Steering}
*                                                                      *
************************************************************************
-}

codeOutput :: DynFlags
           -> Module
           -> FilePath
           -> ModLocation
           -> [(ForeignSrcLang, FilePath)]
           -- ^ additional files to be compiled with with the C compiler
           -> [InstalledUnitId]
           -> IO ForeignStubs
           -> Stream IO RawCmmGroup a                       -- Compiled C--
           -> IO (FilePath,
                  (Bool{-stub_h_exists-}, Maybe FilePath{-stub_c_exists-}),
                  [(ForeignSrcLang, FilePath)]{-foreign_fps-},
                  a)

codeOutput dflags this_mod filenm location foreign_fps pkg_deps genForeignStubs
  cmm_stream
  =
    do  {
        -- Lint each CmmGroup as it goes past
        ; let linted_cmm_stream =
                 if gopt Opt_DoCmmLinting dflags
                    then Stream.mapM do_lint cmm_stream
                    else cmm_stream

              do_lint cmm = withTimingSilent
                  dflags
                  (text "CmmLint"<+>brackets (ppr this_mod))
                  (const ()) $ do
                { case cmmLint dflags cmm of
                        Just err -> do { log_action dflags
                                                   dflags
                                                   NoReason
                                                   SevDump
                                                   noSrcSpan
                                                   (defaultDumpStyle dflags)
                                                   err
                                       ; ghcExit dflags 1
                                       }
                        Nothing  -> return ()
                ; return cmm
                }

        ; a <- case hscTarget dflags of
                 HscAsm         -> outputAsm dflags this_mod location filenm
                                             linted_cmm_stream
                 HscC           -> outputC dflags filenm linted_cmm_stream pkg_deps
                 HscLlvm        -> outputLlvm dflags filenm linted_cmm_stream
                 HscInterpreted -> panic "codeOutput: HscInterpreted"
                 HscNothing     -> panic "codeOutput: HscNothing"
        ; stubs <- genForeignStubs
        ; stubs_exist <- outputForeignStubs dflags this_mod location stubs
        ; return (filenm, stubs_exist, foreign_fps, a)
        }

doOutput :: String -> (Handle -> IO a) -> IO a
doOutput filenm io_action = bracket (openFile filenm WriteMode) hClose io_action

{-
************************************************************************
*                                                                      *
\subsection{C}
*                                                                      *
************************************************************************
-}

outputC :: DynFlags
        -> FilePath
        -> Stream IO RawCmmGroup a
        -> [InstalledUnitId]
        -> IO a

outputC dflags filenm cmm_stream packages
  = do
       withTiming dflags (text "C codegen") (\a -> seq a () {- FIXME -}) $ do

         -- figure out which header files to #include in the generated .hc file:
         --
         --   * extra_includes from packages
         --   * -#include options from the cmdline and OPTIONS pragmas
         --   * the _stub.h file, if there is one.
         --
         let rts = getPackageDetails dflags rtsUnitId

         let cc_injects = unlines (map mk_include (includes rts))
             mk_include h_file =
              case h_file of
                 '"':_{-"-} -> "#include "++h_file
                 '<':_      -> "#include "++h_file
                 _          -> "#include \""++h_file++"\""

         let pkg_names = map installedUnitIdString packages

         doOutput filenm $ \ h -> do
            hPutStr h ("/* GHC_PACKAGES " ++ unwords pkg_names ++ "\n*/\n")
            hPutStr h cc_injects
            Stream.consume cmm_stream (writeC dflags h)

{-
************************************************************************
*                                                                      *
\subsection{Assembler}
*                                                                      *
************************************************************************
-}

outputAsm :: DynFlags -> Module -> ModLocation -> FilePath
          -> Stream IO RawCmmGroup a
          -> IO a
outputAsm dflags this_mod location filenm cmm_stream
 | platformMisc_ghcWithNativeCodeGen $ platformMisc dflags
  = do ncg_uniqs <- mkSplitUniqSupply 'n'

       debugTraceMsg dflags 4 (text "Outputing asm to" <+> text filenm)

       {-# SCC "OutputAsm" #-} doOutput filenm $
           \h -> {-# SCC "NativeCodeGen" #-}
                 nativeCodeGen dflags this_mod location h ncg_uniqs cmm_stream

 | otherwise
  = panic "This compiler was built without a native code generator"

{-
************************************************************************
*                                                                      *
\subsection{LLVM}
*                                                                      *
************************************************************************
-}

outputLlvm :: DynFlags -> FilePath -> Stream IO RawCmmGroup a -> IO a
outputLlvm dflags filenm cmm_stream
  = do {-# SCC "llvm_output" #-} doOutput filenm $
           \f -> {-# SCC "llvm_CodeGen" #-}
                 llvmCodeGen dflags f cmm_stream

{-
************************************************************************
*                                                                      *
\subsection{Foreign import/export}
*                                                                      *
************************************************************************
-}

outputForeignStubs :: DynFlags -> Module -> ModLocation -> ForeignStubs
                   -> IO (Bool,         -- Header file created
                          Maybe FilePath) -- C file created
outputForeignStubs dflags mod location stubs
 = do
   let stub_h = mkStubPaths dflags (moduleName mod) location
   stub_c <- newTempName dflags TFL_CurrentModule "c"

   case stubs of
     NoStubs ->
        return (False, Nothing)

     ForeignStubs h_code c_code -> do
        let
            stub_c_output_d = pprCode CStyle c_code
            stub_c_output_w = showSDoc dflags stub_c_output_d

            -- Header file protos for "foreign export"ed functions.
            stub_h_output_d = pprCode CStyle h_code
            stub_h_output_w = showSDoc dflags stub_h_output_d

        createDirectoryIfMissing True (takeDirectory stub_h)

        dumpIfSet_dyn dflags Opt_D_dump_foreign
                      "Foreign export header file" stub_h_output_d

        -- we need the #includes from the rts package for the stub files
        let rts_includes =
               let rts_pkg = getPackageDetails dflags rtsUnitId in
               concatMap mk_include (includes rts_pkg)
            mk_include i = "#include \"" ++ i ++ "\"\n"

            -- wrapper code mentions the ffi_arg type, which comes from ffi.h
            ffi_includes
              | platformMisc_libFFI $ platformMisc dflags = "#include <ffi.h>\n"
              | otherwise = ""

        stub_h_file_exists
           <- outputForeignStubs_help stub_h stub_h_output_w
                ("#include <HsFFI.h>\n" ++ cplusplus_hdr) cplusplus_ftr

        dumpIfSet_dyn dflags Opt_D_dump_foreign
                      "Foreign export stubs" stub_c_output_d

        stub_c_file_exists
           <- outputForeignStubs_help stub_c stub_c_output_w
                ("#define IN_STG_CODE 0\n" ++
                 "#include <Rts.h>\n" ++
                 rts_includes ++
                 ffi_includes ++
                 cplusplus_hdr)
                 cplusplus_ftr
           -- We're adding the default hc_header to the stub file, but this
           -- isn't really HC code, so we need to define IN_STG_CODE==0 to
           -- avoid the register variables etc. being enabled.

        return (stub_h_file_exists, if stub_c_file_exists
                                       then Just stub_c
                                       else Nothing )
 where
   cplusplus_hdr = "#if defined(__cplusplus)\nextern \"C\" {\n#endif\n"
   cplusplus_ftr = "#if defined(__cplusplus)\n}\n#endif\n"


-- Don't use doOutput for dumping the f. export stubs
-- since it is more than likely that the stubs file will
-- turn out to be empty, in which case no file should be created.
outputForeignStubs_help :: FilePath -> String -> String -> String -> IO Bool
outputForeignStubs_help _fname ""      _header _footer = return False
outputForeignStubs_help fname doc_str header footer
   = do writeFile fname (header ++ doc_str ++ '\n':footer ++ "\n")
        return True

-- | Generate code to initialise info pointer origin
-- See note [Mapping Info Tables to Source Positions]
ipInitCode :: [CmmInfoTable] -> DynFlags -> Module -> InfoTableProvMap -> SDoc
ipInitCode used_info dflags this_mod (InfoTableProvMap closure_map)
 = if not (gopt Opt_InfoTableMap dflags)
    then empty
    else pprCode CStyle $ vcat
    $  map emit_ipe_decl ents
    ++ [emit_ipe_list ents]
    ++ [ text "static void ip_init_" <> ppr this_mod
            <> text "(void) __attribute__((constructor));"
       , text "static void ip_init_" <> ppr this_mod <> text "(void)"
       , braces (vcat
                 [ text "registerInfoProvList" <> parens local_ipe_list_label <> semi
                 ])
       ]
 where
   closure_ents = convertClosureMap used_info this_mod closure_map
   platform = targetPlatform dflags
   ents = closure_ents
   emit_ipe_decl ipe =
       text "extern InfoProvEnt" <+> ipe_lbl <> text "[];"
     where ipe_lbl = pprCLabel dflags (mkIPELabel ipe)
   local_ipe_list_label = text "local_ipe_" <> ppr this_mod
   emit_ipe_list ipes =
      text "static InfoProvEnt *" <> local_ipe_list_label <> text "[] ="
      <+> braces (vcat $ [ pprCLabel dflags (mkIPELabel ipe) <> comma
                         | ipe <- ipes
                         ] ++ [text "NULL"])
      <> semi


{-
Note [Mapping Info Tables to Source Positions]

This note describes what the `-finfo-table-map` flag achieves.

When debugging memory issues it is very useful to be able to map a specific closure
to a position in the source. The prime example is being able to map a THUNK to
a specific place in the source program, the mapping is usually quite precise because
a fresh info table is created for each distinct THUNK.

There are three parts to the implementation

1. In CoreToStg, the SourceNote information is used in order to give a source location to
some specific closures.
2. In StgToCmm, the actually used info tables are recorded.
3. During code generation, a mapping from the info table to the statically
determined location is emitted which can then be queried at runtime by
various tools.

-- Giving Source Locations to Closures

At the moment thunk and constructor closures are added to the map. This information
is collected in the `InfoTableProvMap` which provides a mapping from:

1. Data constructors to a list of where they are used.
2. `Name`s and where they originate from.

During the CoreToStg phase, this map is populated whenever something is turned into
a StgRhsClosure or an StgConApp. The current source position is recorded
depending on the location indicated by the surrounding SourceNote.

The functions which add information to the map are `recordStgIdPosition` and
`incDc`.

When the -fdistinct-constructor-tables` flag is turned on then every
usage of a data constructor gets its own distinct info table. This is orchestrated
in `coreToStgExpr` where an incrementing number is used to distinguish each
occurrence of a data constructor.

-- StgToCmm

The info tables which are actually used in the generated program are recorded during the
conversion from STG to Cmm. The used info tables are recorded in the `emitProc` function.
All the used info tables are recorded in the `cgs_used_info` field. This step
is necessary because when the information about names is collected in the previous
phase it's unpredictable about which names will end up needing info tables. If
you don't record which ones are actually used then you end up generating code
which references info tables which don't exist.

-- Code Generation

The output of these two phases is combined together during code generation.
A C stub is generated which
creates the static map from info table pointer to the information about where that
info table was created from. This is created by `ipInitCode` in the same manner as a
C stub is generated for cost centres.

This information can be consumed in two ways.

1. The complete mapping is emitted into the eventlog so that external tools such
as eventlog2html can use the information with the heap profile by info table mode.
2. The `lookupIPE` function can be used via the `whereFrom#` primop to introspect
information about a closure in a running Haskell program.

Note [Distinct Info Tables for Constructors]

In the old times, each usage of a data constructor used the same info table.
This made it impossible to distinguish which actual usuage of a data constructor was
contributing primarily to the allocation in a program. Using the `-fdistinct-info-tables` flag you
can cause code generation to generate a distinct info table for each usage of
a constructor. Then, when inspecting the heap you can see precisely which usage of a constructor
was responsible for each allocation.

-}
