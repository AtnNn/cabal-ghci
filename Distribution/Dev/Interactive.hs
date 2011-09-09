module Distribution.Dev.Interactive ( 
  cabalSet, packageOpts, loadCabal, lookForCabalFile, withOpts
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

data LoadCabalRet =
  NoCabalFile |
  MissingDeps [Dependency] |
  Pkg FilePath PackageDescription
  deriving Show

pkgDescr (Pkg _ p) = p

compiler = CompilerId buildCompilerFlavor compilerVersion

packageOpts ∷ FilePath → PackageDescription → String → Maybe [String]
packageOpts path pkg executable =
  maybe Nothing (\bi → Just $ includeOpts path bi ++ extensionOpts bi ++ customOpts bi) $
  listToMaybe $
    if executable == ""
    then allBuildInfo pkg
    else
      fmap buildInfo .
      filter (\x → exeName x == executable) .
      executables $ pkg

customOpts bi = 
  hcOptions buildCompilerFlavor bi

extensionOpts bi =
  map (\x → "-X" ++ display x) $ allExtensions bi

includeOpts path bi =
  ["-i" ++ dir ++ "/dist/build/autogen"] ++
  map (("-i"++) . combine dir) (hsSourceDirs bi)
   where dir = takeDirectory path

loadCabal ∷ FilePath → FlagAssignment → IO LoadCabalRet
loadCabal path flags = do
  mCabalFile ← lookForCabalFile =<< canonicalizePath path
  flip (maybe (return NoCabalFile))
    mCabalFile $ \cabalFile → do
    gdescr ← readPackageDescription normal cabalFile
    case finalizePackageDescription flags (const True)
      buildPlatform compiler [] gdescr of
        Left deps → return $ MissingDeps deps
        Right (descr, _) → return $ Pkg cabalFile descr

ifM ∷ Monad m ⇒ m Bool → m a → m a → m a
ifM a b c = a >>= \x → if x then b else c
                                        
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

cabalSet ∷ String → IO String
cabalSet args =
  withOpts (words args)
    (\x → putStrLn x >> return "")
    ((\x → putStrLn x >> return x) .
        unlines . (map (":set "++)) . map show )

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

parseArgs args =
  (map (makeFlag . drop 2) . filter flag $ args, 
   fromMaybe "" . listToMaybe . filter (not . flag) $ args)
  where
    flag x = take 2 x == "-f"
        
makeFlag ('-':f) = (FlagName f, False)
makeFlag f = (FlagName f, True)