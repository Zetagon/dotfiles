-- |
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module XMonad.Actions.Contexts where
import qualified Data.Map.Strict as M
import qualified XMonad.Actions.DynamicProjects as P
import Data.Dynamic
import XMonad hiding (state)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W
import System.IO
import System.Process
import XMonad.Util.Stack(getI)


initializeDynamicProjects :: [Project] -> XConfig a -> XConfig a
initializeDynamicProjects projects =
  P.dynamicProjects ( map _pProject projects )

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


data ContextState = ContextState
  {
    _contexts :: M.Map String Context
  , _currentContext :: String
  } deriving Typeable

currentContext :: ContextState -> Maybe Context
currentContext state = M.lookup (_currentContext state) (_contexts state)

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


-- dmenu :: [String] -> X (String)
-- dmenu xs = io $
--     do
--       (_, Just hout, _, _) <- createProcess (shell ( "echo -e \"" ++ (tail $ concatMap ('\n':) xs) ++ "\" | dmenu")){std_out = CreatePipe}
--       hGetLine hout
