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

data ActiveProjects = AProjects [Project] deriving Typeable
instance ExtensionClass ActiveProjects where
    initialValue = AProjects $ [terminalsP, emacsP, browserP] ++
                   (replicate 7 $ terminalsP)

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
goToProjectNr n = do
  AProjects projects <- XS.get
  switchProject $ projects !! n

switchActiveProjectNr n = do
    switchProjectPrompt def
    project <- currentProject
    AProjects activeProjects <- XS.get :: X ActiveProjects
    XS.put $ AProjects $ switch n project activeProjects

switch :: Int -> a -> [a] -> [a]
switch n x xs = (take n xs) ++ x:(drop (n + 1) xs)
              , projectDirectory = "~/"
              , projectStartHook = Just $ spawn "crunchyroll"}]
projects = mainProjects ++
    [ Project { projectName = "Terminals"
              , projectDirectory = "~/Documents"
              , projectStartHook = Just $ do
                                     replicateM_ 3 $ spawn "konsole"
              }
    ]
