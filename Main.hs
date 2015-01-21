import Bake
import qualified GHC
import GhcMonad
import HscTypes
import Module
import DynFlags
import System.IO
import System.Exit
import System.Directory ( createDirectoryIfMissing )
import qualified Data.Map as Map

--------------------------------------------------------------------------
-- Main

libdir = "/home/ezyang/Dev/ghc-7.10.0.20141222/usr/lib/ghc-7.10.0.20141222"

main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do

    {-
    argv0 <- getArgs
    let (minusB_args, argv1) = partition ("-B" `isPrefixOf`) argv0
        mbMinusB | null minusB_args = Nothing
                 | otherwise = Just (drop 2 (last minusB_args))
    let argv1' = map (GHC.mkGeneralLocated "on the commandline") argv1
    (argv2, staticFlagWarnings) <- parseStaticFlags argv1'
    -}

    GHC.runGhc (Just libdir) $ do
    dflags0 <- GHC.getSessionDynFlags
    let dflags = flip gopt_set Opt_HideAllPackages -- should it be here...
               $ dflags0 { ghcLink = LinkBinary
                         , ghcMode = CompManager
                         , verbosity = 1 }
    GHC.prettyPrintGhcErrors dflags $ do
    GHC.defaultCleanupHandler dflags $ do
    _ <- GHC.setSessionDynFlags dflags
    dflags <- GHC.getSessionDynFlags
    -- HACK! MAKE TRACING WORK
    liftIO $ setUnsafeGlobalDynFlags dflags
    handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
    liftIO $ createDirectoryIfMissing False "dist"
    -- TODO: we need to say WHICH packages to compile, because
    -- some are not eligible (they have holes)

    let pkg_env = mkPackageEnv
                    [ package_q
                    , package_p
                    ]
                    [ ("base", "base-4.8.0.0-e29e6aeb808f5a1814cb5be39ea4f618")
                    ]

    _ <- compile pkg_env (mkPackageName "q") Map.empty
    return ()

--------------------------------------------------------------------------
-- Example data / Sample data

package_p = Backpack {
        backpackName = mkPackageName "p",
        backpackIncludes = [PkgInclude (mkPackageName "base") Nothing],
        backpackExposedModules = [mkModuleName "P"],
        backpackOtherModules = [],
        backpackExposedSignatures = [],
        backpackRequiredSignatures = [mkModuleName "Hole"],
        backpackSourceDir = "p"
    }

package_q = Backpack {
        backpackName = mkPackageName "q",
        backpackIncludes = [ PkgInclude (mkPackageName "base") Nothing
                           , PkgInclude (mkPackageName "base") (Just [(mkModuleName "Data.Word", mkModuleName "Hole")])
                           , PkgInclude (mkPackageName "p") Nothing],
        backpackExposedModules = [mkModuleName "Q"],
        backpackOtherModules = [],
        backpackExposedSignatures = [],
        backpackRequiredSignatures = [],
        backpackSourceDir = "q"
    }

