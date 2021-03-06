{-# LANGUAGE MultiWayIf #-}

module Bake where

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
import Unique

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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Char as Char
import Numeric
import Data.Word
import GHC.Fingerprint
import Data.Ord
import Data.Foldable
import qualified Data.Graph as Graph
import qualified Data.Array as Array

-- BIG TODO: need to be able to register
-- Sort of "half" dealt with by manually shoving PackageConfig
-- in the package database list, but certainly not ideal.

--------------------------------------------------------------------------
-- Backpack data type

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

type PackageType = Set ModuleName -- holes

-- cribbed from Cabal
topologicalOrder :: UniqFM Backpack -> [Backpack]
topologicalOrder m = map toPkgId . Graph.topSort $ graph
    where (graph, toPkgId) = dependencyGraph m

dependencyGraph :: UniqFM Backpack -> (Graph.Graph, Graph.Vertex -> Backpack)
dependencyGraph m = (graph, vertex_to_pkg)
  where
    graph = Array.listArray bounds
                [ [ v | Just v <- map (id_to_vertex.includeName) (backpackIncludes pkg) ]
                | pkg <- pkgs ]
    pkgs             = eltsUFM m
    vertices         = zip (map backpackName pkgs) [0..]
    vertex_map       = Map.fromList vertices
    id_to_vertex pn  = Map.lookup pn vertex_map

    pkgTable = Array.listArray bounds pkgs
    vertex_to_pkg vertex = pkgTable Array.! vertex
    bounds = (0, length pkgs - 1)

--------------------------------------------------------------------------
-- PackageEnv

-- | Information about the Backpack packages that we know about.
data PackageEnv =
    PackageEnv {
          -- | Source package map from 'PackageName' to 'Backpack'
          pkg_sources  :: UniqFM Backpack
          -- | Installed package map from 'PackageName' to
          -- 'InstalledPackageId'.  This map is disjoint from
          -- 'pkg_sources'.
        , pkg_installs :: UniqFM InstalledPackageId
          -- | Derived from 'pkg_sources', this is a map from
          -- 'PackageName' to the holes that a source package requires.
          -- This is a transitively defined property so it's convenient
          -- to pre-compute this before doing any operations.
        , pkg_types    :: UniqFM PackageType
        }

lookupBackpack :: PackageEnv -> PackageName -> Maybe Backpack
lookupBackpack pkg_env = lookupUFM (pkg_sources pkg_env)

lookupHoles :: PackageEnv -> PackageName -> Maybe PackageType
lookupHoles pkg_env = lookupUFM (pkg_types pkg_env)

lookupInstalledPackage :: PackageEnv -> PackageName -> Maybe InstalledPackageId
lookupInstalledPackage pkg_env = lookupUFM (pkg_installs pkg_env)

mkPackageEnv src_list installed_list =
    let src_pkg_db = listToUFM
                   . map (\x -> (backpackName x, x))
                   $ src_list
        installed_db = listToUFM
                     $ [ (mkPackageName k, mkInstalledPackageId v)
                       | (k, v) <- installed_list ]
    in PackageEnv {
        pkg_sources  = src_pkg_db,
        pkg_installs = installed_db,
        pkg_types    = calculateHoles src_pkg_db
    }

--------------------------------------------------------------------------
-- Base 62

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

--------------------------------------------------------------------------
-- Package key

-- | Generates a 'PackageKey' from a 'PackageId', sorted package keys of the
-- immediate dependencies.
mkPackageKey :: PackageName
             -> SourcePackageId
--             -> [PackageKey]     -- dependencies
             -> [(ModuleName, Module)] -- hole instantiations
             -> PackageKey
mkPackageKey (PackageName fsName) (SourcePackageId fsSource) {- deps -} holes =
    fingerprintPackageKey stubName . fingerprintString $
        unpackFS fsSource ++ "\n" ++
        -- NB: packageKeyHash, NOT display
        concat [ moduleNameString m ++ " " ++ packageKeyHash (modulePackageKey b) ++ ":" ++ moduleNameString (moduleName b) ++ "\n"
               | (m, b) <- sortBy (comparing fst) holes] -- ++
        -- CHANGED THIS AGAIN
{-
        concat [ packageKeyHash d ++ "\n"
               | d <- sortBy (comparing packageKeyHash) deps]
               -}
  where stubName = take 5 (filter (/= '-') (unpackFS fsName))

-- MESSED THIS UP.

--------------------------------------------------------------------------
-- Utilities

instance Uniquable PackageName where
    getUnique (PackageName n) = getUnique n

pprKey k = ftext (packageKeyFS k)

convOriginalModule :: DynFlags -> OriginalModule InstalledPackageId ModuleName -> Module
convOriginalModule dflags (OriginalModule a b) = mkModule (resolveInstalledPackageId dflags a) b

mkPackageName = PackageName . mkFastString

mkInstalledPackageId = InstalledPackageId . mkFastString

-- GOOFY
convRenaming Nothing = ModRenaming True []
convRenaming (Just rns) = ModRenaming False [ (moduleNameString orig, moduleNameString new) | (orig, new) <- rns]

--------------------------------------------------------------------------
-- Main

calculateHoles src_pkg_db = foldl' add_trans emptyUFM (topologicalOrder src_pkg_db)
  -- TODO: simplify this by doing a different stage first
  where add_trans m pkg = addToUFM m (backpackName pkg)
                            (Set.fromList (backpackRequiredSignatures pkg
                                ++ backpackExposedSignatures pkg
                                ++ concatMap (add_dep m) (backpackIncludes pkg)))
        add_dep m (PkgInclude n rns)
            | Just holes <- lookupUFM m n = conv_rn holes rns
            | otherwise                   = []
        conv_rn holes Nothing
            = Set.toList holes -- blah
        conv_rn holes (Just rns)
            = mapMaybe (\(orig,new) -> if Set.member orig holes
                                        then Just new
                                        else Nothing) rns

-- TODO drop in a dist directory per package key
-- compile :: Backpack -> -> Ghc ()
compile :: PackageEnv -> PackageName -> Map ModuleName Module -> Ghc PackageConfig
compile pkg_env name hmap
    | Just p <- lookupBackpack pkg_env name
    = compile' pkg_env p hmap
    -- We don't have information on the package, but maybe
    -- it's already installed.  Unfortunately GHC API doesn't give
    -- us a convenient way to query by package name so for now
    -- just hard code everything in
    | Just ipid <- lookupInstalledPackage pkg_env name
    = do dflags <- GHC.getSessionDynFlags
         return (getPackageDetails dflags (resolveInstalledPackageId dflags ipid))
    | otherwise
    = error "can't figure out how to compile it"

addPackage :: PackageConfig -> Ghc ()
addPackage pkg = do
    dflags0 <- GHC.getSessionDynFlags
    case pkgDatabase dflags0 of
        Nothing -> panic "addPackage: called too early"
        Just pkgs -> do let dflags = dflags0 { pkgDatabase = Just (pkg : pkgs) }
                        GHC.setSessionDynFlags dflags
                        liftIO $ setUnsafeGlobalDynFlags dflags
    return ()

compileDep :: PackageEnv
           -> (Map ModuleName Module, [(PackageConfig, PkgInclude)])
           -> PkgInclude
           -> Ghc (Map ModuleName Module, [(PackageConfig, PkgInclude)])
compileDep pkg_env (m, pkgs) incl@(PkgInclude n rns) = do
    dflags <- GHC.getSessionDynFlags
    pkg <- if | Just holes <- lookupHoles pkg_env n
                   -- TODO HANDLE RENAMING
                -> compile pkg_env n (Map.intersection m (Map.fromSet (const ()) holes))
              | Just ipid <- lookupInstalledPackage pkg_env n
                -> return (getPackageDetails dflags (resolveInstalledPackageId dflags ipid))
              | otherwise
                -> error "tuturu"
    dflags <- GHC.getSessionDynFlags
    let add_pkg :: Map ModuleName Module
                -> Maybe [(ModuleName, ModuleName)]
                -> Map ModuleName Module
        add_pkg m Nothing = foldl' add_mod m
                          . map (\e@(ExposedModule n _ _) -> (n, e))
                          $ exposedModules pkg
        add_pkg m (Just rns) = foldl' add_rn_mod m rns
        add_rn_mod m (orig, new) =
            case find (\(ExposedModule n _ _) -> n == orig) (exposedModules pkg) of
                Nothing -> error "oopsies"
                Just e -> add_mod m (new, e)
        add_mod m (bnd, ExposedModule n _ (Just backing))
        -- TODO: ASSERT NO CONFLICT
            = Map.insert bnd (convOriginalModule dflags backing) m
        add_mod m (bnd, ExposedModule n (Just export) Nothing)
            = Map.insert bnd (convOriginalModule dflags export) m
        add_mod m (bnd, ExposedModule n Nothing Nothing)
            = Map.insert bnd (mkModule (packageKey pkg) n) m
    return (add_pkg m rns, (pkg, incl):pkgs)

-- Invariant: hmap contains only PRECISELY the holes you need.  This
-- means we can just slam it and make a package key
compile' :: PackageEnv -> Backpack -> Map ModuleName Module -> Ghc PackageConfig
compile' pkg_env p hmap = do
    let PackageName fs_name = backpackName p
        version = makeVersion [0] -- TODO Grab it from real field
        source_pkg_id = SourcePackageId fs_name -- TODO use version
        key = mkPackageKey (backpackName p) source_pkg_id (Map.toList hmap)

    dflags <- GHC.getSessionDynFlags

    (\k -> case lookupPackage dflags key of
            Just p -> return p
            Nothing -> k) $ do

    -- Compile dependencies
    -- TODO: This is all a bit goofy because we do it again when
    -- we internally process packages.  Probably need to be more
    -- in depth if I want to actually properly setup exposed.
    (_, flags) <- foldM (compileDep pkg_env) (hmap, []) (backpackIncludes p)
    dflags <- GHC.getSessionDynFlags

    pprTrace "Compiling" (ppr (backpackName p) <+> parens (pprKey key)) $ return ()

    let outdir = "dist" </> packageKeyString key
    liftIO $ createDirectoryIfMissing False outdir

    exposed <- withTempSession id $ do
        let targets = map (\m -> Target (TargetModule m) True Nothing)
                          (backpackExposedModules p)
        dflags <- return $ dflags {
                importPaths = [backpackSourceDir p],
                thisPackage = key,
                objectDir   = Just outdir,
                hiDir       = Just outdir,
                stubDir     = Just outdir,
                sigOf       = SigOfMap hmap,
                -- goofy conversion to string
                packageFlags= [ ExposePackage (PackageIdArg (installedPackageIdString pkg))
                                              (convRenaming rns)
                              | (pkg, PkgInclude _ rns) <- flags ],
                includePaths= outdir : includePaths dflags,
                dumpDir     = Just outdir
            }
        pprTrace "sigof" (ppr hmap) $ return ()
        _ <- GHC.setSessionDynFlags dflags

        GHC.setTargets targets
        ok_flag <- GHC.load LoadAllTargets
        when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))

        hsc_env <- getSession

            -- reexport is nice: we can just do a normal module lookup
            -- and use that result.  But can't really do this properly
            -- without the signature import patch.
        let {- lookup_reexport m = do
                r <- liftIO $ findImportedModule hsc_env m Nothing
                case r of
                    Found _ orig -> return [ExposedModule m (Just orig) mb_sig]
                    _ -> return [] -- ACTUALLY SHOULD ERROR HERE
                    -}
            lookup_sig m = case Map.lookup m hmap of
                                Nothing -> error "boop"
                                Just orig ->
                                    let pkg = getPackageDetails dflags (modulePackageKey orig)
                                    in ExposedModule m Nothing (Just (OriginalModule (installedPackageId pkg) (moduleName orig)))
            sigs = map lookup_sig (backpackExposedSignatures p)
            exposed = map (\n -> ExposedModule n Nothing Nothing) (backpackExposedModules p)
                            ++ sigs

        -- Note: must do it here! This is because it only drops home
        -- package modules... and it could only tell this by comparing
        -- thisPackage against the key stored in the table.  Once
        -- we exit the temporary session we lose this designation
        -- and the entries are stuck in the cache.
        -- TODO: Fix this bug in GHC proper
        liftIO $ flushFinderCaches hsc_env
        return exposed

    let pkg = InstalledPackageInfo {
            installedPackageId = InstalledPackageId fs_name, -- TODO: abi hash
            sourcePackageId = source_pkg_id,
            packageName = backpackName p,
            packageVersion = version,
            packageKey = key,
            exposedModules = exposed,
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
    return pkg

-- Possible optimization: when I finish compiling a home package, I can transfer
-- it DIRECTLY to the external package state.  It's a bit delicate
-- though.
