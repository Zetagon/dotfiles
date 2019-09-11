-- |
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Contexts where
import qualified Data.Map.Strict as M
import qualified XMonad.Actions.DynamicProjects as P
import Data.Dynamic
import XMonad
import Control.Lens hiding (Context, contexts)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W

data Project = Project
  {
    _pProject :: P.Project
  , _icon :: Maybe String
  , _iconHighlighted :: Maybe String
  }
makeLenses ''Project

projectName :: Project -> String
projectName p = P.projectName $ _pProject p

data Context = Context
  {
    _allProjects :: [Project]
  , _activeProject :: Project
  , _contextName :: String
  , _contextIcon :: Maybe String
  }
makeLenses ''Context

data ContextState = ContextState
  {
    _contexts :: M.Map String Context
  , _currentContext_ :: Context
  } deriving Typeable
makeLenses ''ContextState

currentContext :: Lens' ContextState Context
currentContext = lens getter setter
  where
    getter state = state^.currentContext_
    setter state context = state
      {
        _contexts = M.adjust (\_ -> state ^. currentContext)
                    (_contextName (state ^. currentContext))
                    $ _contexts state
      , _currentContext_ = context
      }
-- currentContext = lens getter setter
--   where
--     getter state = state^?contexts.at (case (state ^? currentContextName) of
--                                         Just x -> x
--                                         Nothing -> "")
--     setter = undefined

-- currentContext =  prism' up down
--   where
--     up = undefined
--     down = undefined

instance ExtensionClass ContextState where
  initialValue = undefined

goToProjectNr :: Int -> X ()
goToProjectNr n = do
  state <- XS.get
  case (state^?(currentContext.allProjects.(ix n))) of
    Just newProject -> do
      XS.put (set (currentContext.activeProject)
              newProject
              (state :: ContextState))
      XS.put state
      let project_name = (projectName $ state^.currentContext.activeProject)
      windows (\windowset -> W.view project_name
                windowset)
    Nothing -> return()

moveWindowToProjectNr :: Int -> X ()
moveWindowToProjectNr n = do
  state <- XS.get
  case state ^? currentContext . allProjects . ix n . pProject of
    Just targetProject -> P.shiftToProject targetProject
    Nothing -> return ()

switchContextString :: String -> X ()
switchContextString name = undefined
