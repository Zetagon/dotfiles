{-# LANGUAGE DeriveDataTypeable #-}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.DynamicProjects
import XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as XS

import System.IO
import Control.Monad

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/leo/.xmobarrc"
  xmonad $ dynamicProjects projects
         $ defaultConfig
             { manageHook = manageDocks <+> manageHook defaultConfig
             , layoutHook = avoidStruts $ layoutHook defaultConfig
             , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
             , logHook = dynamicLogWithPP xmobarPP
                         { ppOutput = hPutStrLn xmproc
                         , ppTitle = xmobarColor "green" "" . shorten 50}
             , modMask = mod4Mask}
               `additionalKeysP`
               [ ("M4-/", switchProjectPrompt def)
               , ("M4-d", switchActiveProject)
               , ("M4-1", goToActiveProject)
               , ("M4-C-/", shiftToProjectPrompt def)]
               `removeKeysP` ["M4-p"]

data CurrentActiveProject = CAProject Project deriving Typeable
instance ExtensionClass CurrentActiveProject where
    initialValue = CAProject $ head mainProjects

goToActiveProject = do
  CAProject project <- XS.get
  switchProject project

switchActiveProject =  do
    switchProjectPrompt def
    project <- currentProject
    XS.put (CAProject project)

makeEmacsProject name path files =
    Project
      { projectName = name
      , projectDirectory = path
      , projectStartHook = Just $ do
                             spawn "konsole"
                             spawn $ "emacsclient -c " ++ files}

mainProjects =
    [ makeEmacsProject "Emacs" "~/" ""
    , makeEmacsProject "XMonadConfig" "~/.xmonad" "~/.xmonad/xmonad.hs ~/.xmobarrc "
    , Project { projectName = "Watch"
              , projectDirectory = "~/"
              , projectStartHook = Just $ spawn "crunchyroll"}]
projects = mainProjects ++
    [ Project { projectName = "Terminals"
              , projectDirectory = "~/Documents"
              , projectStartHook = Just $ do
                                     replicateM_ 3 $ spawn "konsole"
              }
    ]
