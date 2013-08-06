-- | Retrieve ghci options for your cabal project
module Distribution.Dev.Interactive ( 
  -- * Installing into your .ghci
  -- $install
  
  -- * Arguments
  -- $args
  
  -- * Exported functions
  cabalSet, packageOpts, loadCabal, lookForCabalFile, withOpts, LoadCabalRet(..)
  ) where

import Distribution.Text (display)
import Distribution.Compiler (buildCompilerFlavor, CompilerId(..))
import Distribution.Verbosity (normal)
import Distribution.System (buildPlatform)
import Distribution.Package (PackageName(..), Dependency(..))
import Distribution.PackageDescription (
  FlagName(..), FlagAssignment, BuildInfo(..), hcOptions, allExtensions,
  PackageDescription(..), Executable(..), allBuildInfo)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (
  finalizePackageDescription)
import Distribution.Simple.PackageIndex (lookupDependency)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, installedPkgs)
import Data.Version (Version, showVersion)

import System.FilePath (takeDirectory, (</>), takeExtension)
import System.Directory (
  getDirectoryContents, getCurrentDirectory, canonicalizePath)
import System.Info (compilerVersion)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Exception (try, SomeException)

type Deps = [(PackageName, Version)]

-- | Return value for 'loadCabal'
data LoadCabalRet =
  NoCabalFile | -- ^ No cabal file found
  MissingDeps [Dependency] | -- ^ Missing dependencies
  Pkg FilePath PackageDescription (Maybe LocalBuildInfo) -- ^ Successful loading and parsing of cabal file
  deriving Show

compiler ∷ CompilerId
compiler = CompilerId buildCompilerFlavor compilerVersion

-- | Build a list of ghci options needed to load files from a cabal project
packageOpts
  ∷ FilePath -- ^ path to the .cabal file
  → PackageDescription -- ^ parsed package description
  → Maybe LocalBuildInfo -- ^ parsed build config
  → String -- ^ name of executable
  → Maybe [String]
packageOpts path pkg mlbi executable =
  maybe Nothing (\bi → Just $ ghcOpts path bi (listDeps bi =<< mlbi)) $
  listToMaybe $
    if executable == ""
    then allBuildInfo pkg
    else
      fmap buildInfo .
      filter (\x → exeName x == executable) .
      executables $ pkg

listDeps ∷ BuildInfo → LocalBuildInfo → Maybe Deps
listDeps bi lbi = sequence $ map find reqs 
  where
    reqs = targetBuildDepends bi
    db = installedPkgs lbi
    find dep@(Dependency pkg _) = do
      (fst . head → ver) ← return $ lookupDependency db dep
      return (pkg, ver)

-- | GHC options for a 'BuildInfo'
ghcOpts ∷ FilePath → BuildInfo → Maybe Deps → [String]
ghcOpts path bi deps = concat $ 
  maybe [] ((noPkgs:) . add "-package=" . map addDep) deps :
  map ($ bi) [
  hcOptions buildCompilerFlavor,
  addf "-X" display . allExtensions,
  addf "-i" (dir </>) . ("dist/build/autogen":) . hsSourceDirs,
  add "-optP" . cppOptions,
  add "-optc" . ccOptions,
  add "-optl" . ldOptions
  -- TODO frameworks cSources otherModules extraLibs extraLibsDirs includes 
  ]
  where
    dir = takeDirectory path
    add s = map (s++)
    addf ∷ String → (a → String) → [a] → [String]
    addf s f = map ((s++) . f)
    noPkgs = "-hide-all-packages"
    addDep (PackageName pkg, showVersion → ver) = pkg ++ "-" ++ ver

-- | Load the current cabal project file and parse it
loadCabal
  ∷ FilePath -- ^ usually the current directory
  → FlagAssignment -- ^ list of cabal flag assignments
  → IO LoadCabalRet
loadCabal path flags = do
  mCabalFile ← lookForCabalFile =<< canonicalizePath path
  flip (maybe (return NoCabalFile))
    mCabalFile $ \cabalFile → do
    gdescr ← readPackageDescription normal cabalFile
    mlbi ← loadCabalConfig (takeDirectory cabalFile </> "dist" </> "setup-config")
    case finalizePackageDescription flags (const True)
      buildPlatform compiler [] gdescr of
        Left deps → return $ MissingDeps deps
        Right (descr, _) → return $ Pkg cabalFile descr mlbi

maybeNth ∷ Int → [a] → Maybe a
maybeNth 0 (x:_) = Just x
maybeNth n (_:xs) = maybeNth (n-1) xs
maybeNth _ _ = Nothing


maybeRead ∷ Read a ⇒ String → Maybe a
maybeRead s = case reads s of [(a, "")] → Just a; _ → Nothing

-- | Load the 'LocalBuildInfo' from dist/setup-config
loadCabalConfig ∷ FilePath → IO (Maybe LocalBuildInfo)
loadCabalConfig path = 
  -- TODO: don't ignore all exceptions
  fmap (either ignore id) $ try $
  fmap ((maybeRead =<<) . maybeNth 1 . lines) $ readFile path
  where
    ignore ∷ SomeException → Maybe a
    ignore _ = Nothing

-- | Find a .cabal file in the path or any of it's parent directories
lookForCabalFile
  ∷ FilePath -- ^ canonicalised path
  → IO (Maybe FilePath)
lookForCabalFile "/" = return Nothing
lookForCabalFile path = do
  files ← getDirectoryContents path
  let cabals = filter (\f →
                        takeExtension f == ".cabal"
                        && f /= ".cabal") files
  case cabals of
    [] → lookForCabalFile (takeDirectory path)
    [a] → return $ Just $ path </> a
    _ → return Nothing

-- | 'cabalSet' returns a list of ghci commands (seperated by newlines) that :set the 'packageOpts' of the current cabal project
cabalSet
  ∷ String -- ^ arguments seperated by spaces
  → IO String
cabalSet args =
  withOpts (words args)
    (\x → putStrLn x >> return "")
    ((\x → putStrLn x >> return x) .
        unlines . (map (":set "++)) . map show )

-- | Generalised version of 'cabalSet'
withOpts
  ∷ [String] -- ^ List of cabal flag arguments and executable name
  → (String → IO a) -- ^ Error continuation. Recieves an error message.
  → ([String] → IO a) -- ^ Success continuation. Recieves a list of ghci arguments.
  → IO a
withOpts args err go = do
  let (flags, executable) = parseArgs args
  here ← getCurrentDirectory
  ret ← loadCabal here flags
  case ret of
    NoCabalFile → err "Current directory is not a cabal project"
    MissingDeps deps → err $ "Missing dependencies: " ++ unwords (map show deps)
    Pkg path descr mlbi → do
      let mopts = packageOpts path descr mlbi executable
      case mopts of
        Nothing → err (
          if executable /= ""
          then "No such executable in cabal file"
          else "No library defined in cabal file")
        Just opts → go opts

parseArgs ∷ [String] → (FlagAssignment, String)
parseArgs args =
  (map (makeFlag . drop 2) . filter flag $ args, 
   fromMaybe "" . listToMaybe . filter (not . flag) $ args)
  where
    flag x = take 2 x == "-f"
        
makeFlag ∷ String → (FlagName, Bool)
makeFlag ('-':f) = (FlagName f, False)
makeFlag f = (FlagName f, True)

-- $install
-- @
--   $ head -n 4 >> ~/.ghci
--   :m + Distribution.Dev.Interactive
--   :def cabalset cabalSet
--   :cabalset -fcabal-ghci
--   :m - Distribution.Dev.Interactive
-- @
-- 
-- After you've added those lines into your .ghci file, use the @:cabalset@
-- command to reload your .cabal file.

-- $args
-- [@-fflag@] enable flag
-- 
-- [@-f-flag@] disable flag
-- 
-- [@exec@] load options for the exec executable

