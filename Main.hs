import qualified GHC
import GHC (Ghc, GhcMonad(..), LoadHowMuch(..))
import GhcMonad

import GHC.PackageDb

import Finder
import Outputable
import HscTypes
import BasicTypes
import Module
import FastString
import PackageConfig
import DynFlags
import MonadUtils       ( liftIO )
import Panic
import Packages
import UniqFM
import StaticFlags

import Data.IORef
import Data.Version
import System.IO
import System.Environment
import System.Exit
import System.FilePath
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Char as Char
import Numeric
import Data.Word
import GHC.Fingerprint
import Data.Ord

-- BIG TODO: need to be able to register

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
    let dflags = id -- flip dopt_set Opt_D_ppr_debug
               $ dflags0 { ghcLink = LinkBinary
                         , ghcMode = CompManager
                         , verbosity = 1 }
    GHC.prettyPrintGhcErrors dflags $ do
    GHC.defaultCleanupHandler dflags $ do
    _ <- GHC.setSessionDynFlags dflags
    handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
    liftIO $ createDirectoryIfMissing False "dist"
    -- TODO: we need to say WHICH packages to compile, because
    -- some are not eligible (they have holes)
    compile package_p
    compile package_q

data Backpack
    = Backpack {
        backpackName :: PackageName,
        backpackIncludes :: [PkgInclude],
        backpackExposedModules :: [ModuleName],
        backpackOtherModules :: [ModuleName],
        backpackExposedSignatures :: [ModuleName],
        backpackRequiredSignatures :: [ModuleName],
        backpackSourceDir :: FilePath
    }

data PkgInclude = PkgInclude {
        includeName :: PackageName,
        includeRenaming :: Maybe [(ModuleName, ModuleName)]
    }



package_p = Backpack {
        backpackName = mkPackageName "p",
        backpackIncludes = [PkgInclude (mkPackageName "base") Nothing],
        backpackExposedModules = [mkModuleName "P"],
        backpackOtherModules = [],
        backpackExposedSignatures = [],
        backpackRequiredSignatures = [],
        backpackSourceDir = "p"
    }

package_q = Backpack {
        backpackName = mkPackageName "q",
        backpackIncludes = [ PkgInclude (mkPackageName "base") Nothing
                           , PkgInclude (mkPackageName "p") Nothing],
        backpackExposedModules = [mkModuleName "Q"],
        backpackOtherModules = [],
        backpackExposedSignatures = [],
        backpackRequiredSignatures = [],
        backpackSourceDir = "q"
    }

mkPackageName = PackageName . mkFastString

-- compiles with:
--  ghc --make P -this-package-key p_KEY

addPackage :: PackageConfig -> Ghc ()
addPackage pkg = do
    dflags0 <- GHC.getSessionDynFlags
    case pkgDatabase dflags0 of
        Nothing -> panic "addPackage: called too early"
        Just pkgs -> do let dflags = dflags0 { pkgDatabase = Just (pkg : pkgs) }
                        GHC.setSessionDynFlags dflags
    return ()

-- The base-62 code is based off of 'locators'
-- ((c) Operational Dynamics Consulting, BSD3 licensed)

-- Note: Instead of base-62 encoding a single 128-bit integer
-- (ceil(21.49) characters), we'll base-62 a pair of 64-bit integers
-- (2 * ceil(10.75) characters).  Luckily for us, it's the same number of
-- characters!  In the long term, this should go in GHC.Fingerprint,
-- but not now...

-- | Size of a 64-bit word when written as a base-62 string
word64Base62Len :: Int
word64Base62Len = 11

-- | Converts a 64-bit word into a base-62 string
toBase62 :: Word64 -> String
toBase62 w = pad ++ str
  where
    pad = replicate len '0'
    len = word64Base62Len - length str -- 11 == ceil(64 / lg 62)
    str = showIntAtBase 62 represent w ""
    represent :: Int -> Char
    represent x
        | x < 10 = Char.chr (48 + x)
        | x < 36 = Char.chr (65 + x - 10)
        | x < 62 = Char.chr (97 + x - 36)
        | otherwise = error ("represent (base 62): impossible!")

packageKeyHash :: PackageKey -> String
packageKeyHash pk =
    let s = packageKeyString pk in
    case dropWhile (/='_') s of
        [] -> s
        (_:rest) -> rest

fingerprintPackageKey :: String -> Fingerprint -> PackageKey
fingerprintPackageKey s (Fingerprint a b) = stringToPackageKey (s ++ "_" ++ toBase62 a ++ toBase62 b)

-- | Generates a 'PackageKey' from a 'PackageId', sorted package keys of the
-- immediate dependencies.
mkPackageKey :: PackageName
             -> SourcePackageId
             -> [PackageKey]     -- dependencies
             -> [(ModuleName, (PackageKey, ModuleName))] -- hole instantiations
             -> PackageKey
mkPackageKey (PackageName fsName) (SourcePackageId fsSource) deps holes =
    fingerprintPackageKey stubName . fingerprintString $
        unpackFS fsSource ++ "\n" ++
        -- NB: packageKeyHash, NOT display
        concat [ moduleNameString m ++ " " ++ packageKeyHash p' ++ ":" ++ moduleNameString m' ++ "\n"
               | (m, (p', m')) <- sortBy (comparing fst) holes] ++
        concat [ packageKeyHash d ++ "\n"
               | d <- sortBy (comparing packageKeyHash) deps]
  where stubName = take 5 (filter (/= '-') (unpackFS fsName))

-- | Compiles only if it isn't already compiled
maybeCompile :: Backpack -> Ghc ()
maybeCompile p = do
    undefined

-- TODO drop in a dist directory per package key
compile :: Backpack -> Ghc ()
compile p = do
    let PackageName name = backpackName p
    -- OK time to knit everything together

    -- TODO: properly calculate
    let key = fsToPackageKey name
        outdir = "dist" </> packageKeyString key
    liftIO $ createDirectoryIfMissing False outdir
    withTempSession id $ do -- drop the package stuff
        let targets = map (\m -> Target (TargetModule m) True Nothing)
                          (backpackExposedModules p)
        dflags0 <- GHC.getSessionDynFlags
        -- XXX: control package visibility properly
        let dflags = dflags0 {
                importPaths = [backpackSourceDir p],
                thisPackage = key,
                objectDir   = Just outdir,
                hiDir       = Just outdir,
                stubDir     = Just outdir,
                includePaths= outdir : includePaths dflags0,
                dumpDir     = Just outdir
            }
        _ <- GHC.setSessionDynFlags dflags

        GHC.setTargets targets
        ok_flag <- GHC.load LoadAllTargets
        when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))

        -- Note: must do it here! This is because it only drops home
        -- package modules... and it could only tell this by comparing
        -- thisPackage against the key stored in the table.  Once
        -- we exit the temporary session we lose this designation
        -- and the entries are stuck in the cache.
        hsc_env <- getSession
        liftIO $ flushFinderCaches hsc_env
    let version = makeVersion [0]
        pkg = InstalledPackageInfo {
            installedPackageId = InstalledPackageId name, -- TODO: abi hash
            sourcePackageId = SourcePackageId name, -- TODO: plus version
            packageName = backpackName p,
            packageVersion = version,
            packageKey = key,
            exposedModules =
                map (\n -> ExposedModule n Nothing Nothing) (backpackExposedModules p),
            hiddenModules = [], -- TODO: doc only
            instantiatedWith = [], -- TODO (not used by anything right now)
            depends = [], -- ???
            importDirs = [outdir],
            exposed = True,
            -- nope
            hsLibraries = [],
            extraLibraries = [],
            extraGHCiLibraries = [],
            libraryDirs = [],
            frameworks = [],
            frameworkDirs = [],
            ldOptions = [],
            ccOptions = [],
            includes = [],
            includeDirs = [],
            haddockInterfaces = [],
            haddockHTMLs = [],
            trusted = False
        }
    addPackage pkg

-- Possible optimization: when I finish compiling a home package, I can transfer
-- it DIRECTLY to the external package state.  It's a bit delicate
-- though.
