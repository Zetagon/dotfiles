-- * Imports
{-# LANGUAGE DeriveDataTypeable #-}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP, removeKeysP)
import XMonad.Actions.DynamicProjects
import XMonad.Prompt (mkXPrompt, mkComplFunFromList)
import XMonad.Prompt.Input
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.NamedScratchpad
import XMonad.Layout.ThreeColumns

import System.IO
import Control.Monad

-- * Entry point
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/leo/.xmobarrc" -- Start xmobar with config
  xmonad $ dynamicProjects projects -- Launch dynamic projects
         $ def
             { manageHook = manageDocks <+> manageHook def
             , layoutHook = myLayouts
             , handleEventHook = myEvents
             , logHook = myLog xmproc
             , modMask = mod4Mask}
               `additionalKeysP`
               myKeymap
               `removeKeysP` ["M4-p"] -- Reserved for rofi using xbindkeys
-- * Layouts
myLayouts = avoidStruts $  (Tall 1 0.03 0.80) ||| ThreeColMid 1 0.03 0.5 -- Define two layouts
-- * Events
myEvents = handleEventHook def <+> docksEventHook
-- * Logging to xmobar
myLog xmproc = dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "green" "" . shorten 50}
-- * Keymaps
myKeymap = ([ ("M4-/", switchProjectPrompt def)
                , ("M4-C-/", shiftToProjectPrompt def)]
               ++ map (\x -> ("M4-S-" ++ show x, switchActiveProjectNr x)) [0..9] -- Go to the project at position x
               ++ map (\x-> ("M4-" ++ show x, goToProjectNr x)) [0..9]) -- Assign a project to position x
-- * Dynamic Projects
-- ** Project list
emacsP =  makeEmacsProject "Emacs" "~/" ""

xmonadConfigP = makeEmacsProject "XMonadConfig" "~/.xmonad" "~/.xmonad/xmonad.hs ~/.xmobarrc "

watchP = Project { projectName = "Watch"
              , projectDirectory = "~/"
              , projectStartHook = Just $ spawn "crunchyroll"}
keepassP = makeSimpleProject "Mail" ["keepassx"]
thunderbirdP = makeSimpleProject "Mail" ["thunderbird"]

terminalsP = Project { projectName = "Terminals"
              , projectDirectory = "~/Documents"
              , projectStartHook = Just $ do
                                     replicateM_ 3 $ spawn "konsole"
              }

browserP = makeSimpleProject "Browser" ["firejail --noprofile firefox"]

messagingP = makeSimpleProject "Messaging" ["discord", "slack"]
projects = [ terminalsP
           , browserP
           , emacsP
           , xmonadConfigP
           , watchP
           , thunderbirdP
           , messagingP]

defaultProjectList = [ terminalsP -- 0
                     , emacsP -- 1
                     , browserP -- 2
                     , keepassP -- 3
                     , terminalsP -- 4
                     , thunderbirdP -- 5
                     , terminalsP -- 6
                     , terminalsP -- 7
                     , terminalsP -- 8
                     , messagingP -- 9
                     ]
-- ** Project utility functions
makeEmacsProject name path files =
    Project
      { projectName = name
      , projectDirectory = path
      , projectStartHook = Just $ do
                             spawn $ "emacsclient -c " ++ files
                             spawn "konsole"
      }

makeSimpleProject name programs =
    Project { projectName = name
            , projectDirectory = "~/"
            , projectStartHook = Just $
                                   mapM_ spawn programs
            }
-- ** Active Projects
-- TODO Make a better name for this
data ActiveProjects = AProjects [Project] deriving Typeable
instance ExtensionClass ActiveProjects where
    initialValue = AProjects $ defaultProjectList



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

