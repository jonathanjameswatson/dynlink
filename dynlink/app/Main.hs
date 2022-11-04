module Main (main) where

import GHC (GhcLink (LinkInMemory), HscEnv, ModuleName, defaultErrorHandler, getSession, getSessionDynFlags, mkModule, mkModuleName, noSrcSpan, runGhc, setSessionDynFlags)
import GHC.Driver.Monad (liftIO)
import GHC.Driver.Session (DynFlags, defaultFatalMessager, defaultFlushOut, ghcLink, initDefaultSDocContext)
import qualified GHC.Driver.Session as Session
import GHC.Driver.Types (hsc_dynLinker)
import GHC.IO.Handle.FD (stdout)
import GHC.Paths (libdir)
import GHC.Runtime.Linker (getHValue, initDynLinker, linkPackages, showLinkerState)
import GHC.Types.Name (mkExternalName, mkVarOcc)
import GHC.Types.Unique (mkBuiltinUnique)
import GHC.Unit.Info (UnitInfo, mkUnit, pprUnitInfo)
import qualified GHC.Unit.Info as Info
import GHC.Unit.State (lookupModuleInAllUnits)
import GHC.Utils.Outputable (SDoc, printSDoc)
import GHC.Utils.Ppr (Mode (PageMode))
import Unsafe.Coerce (unsafeCoerce)

load ::
  String ->
  String ->
  IO Int
load moduleNameString symbol = do
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      flags' <- getSessionDynFlags
      let waysCorrectedFlags = flags' {ghcLink = LinkInMemory}
      _linkerPackages <- setSessionDynFlags waysCorrectedFlags
      flags <- getSessionDynFlags

      session <- getSession
      liftIO $ initDynLinker session
      liftIO $ printLinkerState session flags

      liftIO $ putStrLn "-----------"

      let moduleName = mkModuleName moduleNameString
      unitInfo <- liftIO $ lookupUnitInfo flags moduleName
      liftIO $ prettyPrint flags $ pprUnitInfo unitInfo

      liftIO $ putStrLn "-----------"

      liftIO $ linkPackages session [Info.unitId unitInfo]
      liftIO $ printLinkerState session flags

      liftIO $ putStrLn "-----------"

      let unit = mkUnit unitInfo
      let module_ =
            mkModule
              unit
              moduleName

      -- liftIO $ linkModule session module_

      let name =
            mkExternalName
              (mkBuiltinUnique 0)
              module_
              (mkVarOcc symbol)
              noSrcSpan

      value <- liftIO $ getHValue session name
      return $ unsafeCoerce value

lookupUnitInfo :: DynFlags -> ModuleName -> IO UnitInfo
lookupUnitInfo flags moduleName = do
  let unitState = Session.unitState flags
      modulesAndPackages = lookupModuleInAllUnits unitState moduleName
      exposedModulesAndPackages = filter (\(_, unitInfo) -> Info.unitIsExposed unitInfo) modulesAndPackages
  case exposedModulesAndPackages of
    [] -> do
      error "Couldn't find module"
    (_, unitInfo) : _ ->
      return unitInfo

prettyPrint :: DynFlags -> SDoc -> IO ()
prettyPrint flags sdoc = do
  let sdocContext = initDefaultSDocContext flags
  printSDoc sdocContext PageMode stdout sdoc
  putStrLn ""

printLinkerState :: HscEnv -> DynFlags -> IO ()
printLinkerState session flags = do
  let linker = hsc_dynLinker session
  sdoc <- showLinkerState linker
  prettyPrint flags sdoc

main :: IO ()
main = do
  x <- load "ExamplePackageLib" "x"
  print x
