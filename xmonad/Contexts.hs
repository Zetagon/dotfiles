-- |
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module XMonad.Actions.Contexts where
import Codec.Binary.UTF8.String (encodeString)
import qualified Data.Map.Strict as M
import qualified XMonad.Actions.DynamicProjects as P
import Data.Dynamic
import XMonad hiding (state)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W
import System.IO
import System.Process
import XMonad.Util.Stack(getI)
import XMonad.Hooks.DynamicLog
import System.IO
import System.Process
import XMonad.Util.Run(spawnPipe, safeSpawn, hPutStr)
import Data.List (intersperse, isInfixOf)
import XMonad.Util.NamedWindows (getName)


initializeDynamicProjects :: [Context] -> Handle -> XConfig a -> XConfig a
initializeDynamicProjects contexts xmproc c =
  P.dynamicProjects ( map _pProject $ concat $ map _allProjects contexts ) c
  { startupHook = contextStartupHook contexts <> startupHook c
  , logHook = contextLogHook xmproc <> logHook c
  }

contextLogHook :: Handle -> X()
contextLogHook xmproc =
  do
    io . hPutStrLn xmproc =<< dynamicLogStringContext xmobarPP

      where
        sepBy sep = concat . intersperse sep . filter (not . null)

        -- TODO Respect configurations by PP
        -- Like dynamicLogString but for project contexts
        dynamicLogStringContext :: PP -> X String
        dynamicLogStringContext pp = do

          state' <- XS.get -- get a list of projects in this context
          let context' = case state' of
                Nothing -> Nothing
                Just state -> currentContext state
          case context' of
            Nothing -> return ""
            Just context ->
              do
                current_project_name <- getCurrentProjectNameX -- get the project that is currently focused

                winset <- gets windowset

                -- layout description
                let ld = description . W.layout . W.workspace . W.current $ winset

                -- workspace list
                let ws =
                      concat $
                      intersperse " " $
                      map (\name -> if current_project_name == name
                                    then xmobarColor "#bd58f4" "#3d383f" name
                                    else name) $
                      map ( P.projectName . _pProject)
                      $ _allProjects context
                  in
                  do
                    -- window title
                    wt <- (maybe (return "") (fmap show . getName) . W.peek $ winset)

                    -- run extra loggers, ignoring any that generate errors.
                    extras <- mapM (flip catchX (return Nothing)) $ ppExtras pp

                    return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
                      [ ws
                      , ppLayout pp ld
                      , ppTitle  pp $ ppTitleSanitize pp wt
                      ]
contextStartupHook :: [Context] -> X()
contextStartupHook contexts =
  do
    XS.put $ Just $
      ContextState { _currentContext = _contextName $ head contexts
                   , _contexts = M.fromList $ zip (map _contextName contexts) contexts
                   }

data Project = Project
  {
    _pProject :: P.Project
  , _icon :: Maybe String
  , _iconHighlighted :: Maybe String
  }

projectName :: Project -> String
projectName p = P.projectName $ _pProject p


data Context = Context
  {
    _allProjects :: [Project]
  , _activeProject :: Project
  , _contextName :: String
  , _contextIcon :: Maybe String
  }

mkContext :: String -> [Project] -> Context
mkContext name projects =
  Context { _allProjects = projects
          , _activeProject = head projects
          , _contextName = name
          , _contextIcon = Nothing
          }

data ContextState = ContextState
  {
    _contexts :: M.Map String Context
  , _currentContext :: String
  } deriving Typeable

currentContext :: ContextState -> Maybe Context
currentContext state = M.lookup (_currentContext state) (_contexts state)

currentContextU :: ContextState -> Context
currentContextU state =
  case currentContext state of
    Just x -> x

setCurrentContext :: ContextState -> String -> ContextState
setCurrentContext  state str =
  state { _currentContext = str }

mapCurrentContext :: ContextState -> (Context -> Context) -> ContextState
mapCurrentContext state f =
     state
     {
       _contexts = M.adjust f (_currentContext state)$ _contexts state
     }

getProjectNr :: ContextState -> Int -> Maybe Project
getProjectNr state n =
  do
    context <- currentContext state
    getI n (_allProjects context)

setProject :: ContextState -> Int -> ContextState
setProject state index =
  mapCurrentContext state $
  \context -> case getI index (_allProjects context) of
    Just project -> context { _activeProject = project }
    Nothing -> context

getCurrentProjectName :: ContextState -> String
getCurrentProjectName state =
  P.projectName $ getCurrentDynamicProject state

getCurrentProjectNameX :: X (String)
getCurrentProjectNameX = do
  state' <- XS.get
  case state' of
    Just state -> return $ getCurrentProjectName state
    Nothing -> return ""


getCurrentDynamicProject :: ContextState -> P.Project
getCurrentDynamicProject state =
  _pProject $ _activeProject $ case currentContext state of
                                 Nothing -> undefined
                                 Just x -> x
goToProjectNr :: Int -> X ()
goToProjectNr n =
  do
    Just state <- XS.get
    let newState = setProject state n
    XS.put $ Just $ newState
    P.switchProject (getCurrentDynamicProject newState)
--    windows (\wset ->
 --              W.view (getCurrentProjectName newState) wset)



shiftToProjectNr :: Int -> X()
shiftToProjectNr n = do
    Just state <- XS.get
    P.shiftToProject $ _pProject $ case getProjectNr state n of
                                     Just x -> x

switchContext :: X ()
switchContext =
  do
    Just state <- XS.get
    let contexts = M.keys $ _contexts state
    chosenContext <- dmenu $ (contexts)
    let chosen_state = setCurrentContext state chosenContext
    XS.put $ Just chosen_state
    P.switchProject (getCurrentDynamicProject chosen_state)


instance ExtensionClass (Maybe ContextState) where
  initialValue = Nothing

-- goToProjectNr :: Int -> X ()
-- goToProjectNr n = do
--   Just state <- XS.get
--   case (state^?(currentContext.allProjects.(ix n))) of
--     Just newProject -> do
--       XS.put $ Just $ (set (currentContext.activeProject)
--                      newProject
--                      (state :: ContextState))
--       XS.put $ Just state
--       let project_name = (projectName $ state^.currentContext.activeProject)
--       windows (\windowset -> W.view project_name
--                 windowset)
--     Nothing -> return()

-- moveWindowToProjectNr :: Int -> X ()
-- moveWindowToProjectNr n = do
--   Just state <- XS.get
--   case state ^? currentContext . allProjects . ix n . pProject of
--     Just targetProject -> P.shiftToProject targetProject
--     Nothing -> return ()

-- switchContextString :: String -> X ()
-- switchContextString name =
--   do
--     Just state <- XS.get
--     case (state ^. contexts . at name) of
--       Just newContext -> XS.put $ Just $ state & currentContext .~ newContext
--       Nothing -> return ()

-- switchContextDmenu :: X ()
-- switchContextDmenu =
--   do
--     Just state <- XS.get
--     contextName <- dmenu $ state ^. contexts . (to M.keys)
--     switchContextString contextName


dmenu :: [String] -> X (String)
dmenu xs = io $
    do
      (_, Just hout, _, _) <- createProcess (shell ( "echo -e \"" ++ (tail $ concatMap ('\n':) xs) ++ "\" | dmenu")){std_out = CreatePipe}
      hGetLine hout
