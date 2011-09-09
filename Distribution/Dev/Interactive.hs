-- | Retrieve ghci options for your cabal project
module Distribution.Dev.Interactive ( 
  -- * Installing into your .ghci
  -- $install
  
  -- * Arguments
  -- $args
  
  -- * Exported functions
  cabalSet, packageOpts, loadCabal, lookForCabalFile, withOpts, LoadCabalRet(..)
  ) where

import Distribution.Text
import Distribution.Compiler
import Distribution.Verbosity
import Distribution.System
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration

import System.FilePath
import System.Directory
import System.Info
import Data.Maybe

-- | Return value for 'loadCabal'
data LoadCabalRet =
  NoCabalFile | -- ^ No cabal file found
  MissingDeps [Dependency] | -- ^ Missing dependencies
  Pkg FilePath PackageDescription -- ^ Successful loading and parsing of cabal file
  deriving Show

compiler ∷ CompilerId
compiler = CompilerId buildCompilerFlavor compilerVersion

-- | Build a list of ghci options needed to load files from a cabal project
packageOpts
  ∷ FilePath -- ^ path to the .cabal file
  → PackageDescription -- ^ parsed package description
  → String -- ^ name of executable
  → Maybe [String]
packageOpts path pkg executable =
  maybe Nothing (\bi → Just $ includeOpts path bi ++ extensionOpts bi ++ customOpts bi) $
  listToMaybe $
    if executable == ""
    then allBuildInfo pkg
    else
      fmap buildInfo .
      filter (\x → exeName x == executable) .
      executables $ pkg

customOpts ∷ BuildInfo → [String]
customOpts bi = 
  hcOptions buildCompilerFlavor bi

extensionOpts ∷ BuildInfo → [String]
extensionOpts bi =
  map (\x → "-X" ++ display x) $ allExtensions bi

includeOpts ∷ FilePath → BuildInfo → [String]
includeOpts path bi =
  ["-i" ++ dir ++ "/dist/build/autogen"] ++
  map (("-i"++) . combine dir) (hsSourceDirs bi)
   where dir = takeDirectory path

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
    case finalizePackageDescription flags (const True)
      buildPlatform compiler [] gdescr of
        Left deps → return $ MissingDeps deps
        Right (descr, _) → return $ Pkg cabalFile descr

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
    [a] → return $ Just $ combine path a
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
    Pkg path descr → do
      let mopts = packageOpts path descr executable
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
--   :cabalset
--   :m - Distribution.Dev.Interactive
-- @

-- $args
-- [@-fflag@] enable flag
-- 
-- [@-f-flag@] disable flag
-- 
-- [@exec@] load options for the exec executable

