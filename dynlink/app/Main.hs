module Main (main) where

import GHC (GhcLink (LinkInMemory), ModuleName, defaultErrorHandler, getSession, getSessionDynFlags, mkModule, mkModuleName, noSrcSpan, runGhc, setSessionDynFlags)
import GHC.Driver.Monad (liftIO)
import GHC.Driver.Session (DynFlags (ghcMode), GhcMode (CompManager), addWay', defaultFatalMessager, defaultFlushOut, ghcLink)
import qualified GHC.Driver.Session as Session
import GHC.Driver.Ways (Way (WayDyn))
import GHC.Paths (libdir)
import GHC.Runtime.Interpreter (withInterp, wormhole)
import GHC.Runtime.Linker (getHValue, initDynLinker, linkPackages)
import GHC.Types.Name (mkExternalName, mkVarOcc)
import GHC.Types.Unique (mkBuiltinUnique)
import GHC.Unit.Info (UnitInfo, mkUnit)
import qualified GHC.Unit.Info as Info
import GHC.Unit.State (lookupModuleInAllUnits)
import Unsafe.Coerce (unsafeCoerce)

load ::
  String ->
  String ->
  IO Int
load moduleNameString symbol = do
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      flags' <- getSessionDynFlags
      _ <- setSessionDynFlags $ addWay' WayDyn $ flags' {ghcMode = CompManager, ghcLink = LinkInMemory}
      flags <- getSessionDynFlags

      session <- getSession
      liftIO $ initDynLinker session

      let moduleName = mkModuleName moduleNameString
      unitInfo <- liftIO $ lookupUnitInfo flags moduleName

      liftIO $ linkPackages session [Info.unitId unitInfo]

      let unit = mkUnit unitInfo
      let module_ =
            mkModule
              unit
              moduleName
      let name =
            mkExternalName
              (mkBuiltinUnique 0)
              module_
              (mkVarOcc symbol)
              noSrcSpan

      value <- liftIO $ withInterp session $ \interp -> getHValue session name >>= wormhole interp
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

main :: IO ()
main = do
  x <- load "ExamplePackageLib" "x"
  print x
