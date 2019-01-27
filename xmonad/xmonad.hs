-- * Imports
{-# LANGUAGE DeriveDataTypeable #-}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP, removeKeysP)
import XMonad.Actions.DynamicProjects
import XMonad.Prompt (mkXPrompt, mkComplFunFromList, XPrompt, showXPrompt)
import XMonad.Prompt.Input
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.NamedScratchpad
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad
import System.IO
import Control.Monad

import GHC.Exts (sortWith)

-- * Entry point
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/leo/.xmobarrc" -- Start xmobar with config
  xmonad $ dynamicProjects projects -- Launch dynamic projects
         $ def
             { manageHook = myManageHook
             , layoutHook = myLayouts
             , handleEventHook = myEvents
             , logHook = myLog xmproc
             , modMask = mod4Mask}
               `additionalKeysP`
               myKeymap
               `removeKeysP` ["M4-p"] -- Reserved for rofi using xbindkeys
-- * ManageHook
myManageHook = namedScratchpadManageHook scratchpads <+> manageDocks <+> manageHook def
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
            , ("M4-C-/", shiftToProjectPrompt def)
            , ("M4-d", XS.put $ AProjects defaultProjectList)
            , ("M4-s", switchSuperProject)
            , ("M4-t", namedScratchpadAction scratchpads "htop")]
               ++ map (\x -> ("M4-S-" ++ show x, switchActiveProjectNr x)) [0..9] -- Go to the project at position x
               ++ map (\x-> ("M4-" ++ show x, goToProjectNr x)) [0..9]) -- Assign a project to position x
-- * Scratchpads

scratchpads =
-- run htop in xterm, find it by title, use default floating window placement
    [ NS "htop" "xterm -e htop" (title =? "htop") defaultFloating ]
-- * Project list
projects = [ makeEmacsProject "Emacs" "~/" ""
           , makeEmacsProject "XMonadConfig" "~/.xmonad" "~/.xmonad/xmonad.hs ~/.xmobarrc "
           , makeSimpleProject "XMonad Browser" ["firejail --noprofile firefox - --new-window"]
           , makeEmacsProject "Spirited Away" "~/Documents/texter/analys-spirited-away" "~/Documents/texter/analys-spirited-away/master.tex"
           , makeSimpleProject "Spirited Away Browser" ["firejail --noprofile firefox - --new-window"]
           , makeSimpleProject "Watch" ["crunchyroll"]
           , makeSimpleProject "Keepass" ["keepassx"]
           , makeSimpleProject "Mail" ["thunderbird"]
           , makeSimpleProject "Terminals" $ replicate 3 "konsole"
           , makeSimpleProject "Browser" ["firejail --noprofile firefox"]
           , makeSimpleProject "Messaging" ["firefox --new-window https://discordapp.com/login", "slack"]
           , makeSimpleProject "VLC" ["vlc"]
           , makeSimpleProject "Mpsyt" ["xterm -e \"firejail --noprofile mpsyt\""]
           , makeSimpleProject "Zotero" ["zotero"]
           ]

terminalsP = getP "Terminals"




defaultProjectList = mkProjectList [ (1, "Emacs")
                                   , (2, "Browser")
                                   , (5, "Mail")
                                   , (8, "Keepass")
                                   , (9, "Messaging")]
-- * Super Project List
defaultSuperProjectList = [ ("default" , defaultProjectList)
                          , ("watch", mkProjectList [(1, "Watch"), (8, "Keepass")])
                          , ("spirited away", mkProjectList [(1, "Spirited Away"), (2, "Spirited Away Browser"), (0, "Mpsyt")])
                          , ("xmonad", mkProjectList [ (1, "XMonadConfig")
                                                     , (2, "XMonad Browser")
                                                     , (0, "Mpsyt")])
                          , ("browser", mkProjectList [(2, "Browser")])
                          ]
-- * Code
-- ** Dynamic Project utility functions
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

-- UNSAFE! Only use this with literals as a string that is not in projects will crash XMonad
getP = getProject projects
    where getProject :: [Project] -> String -> Project
          getProject xs name = head $ filter (\x -> projectName x == name) xs


-- ** Active Projects
-- TODO Make a better name for this
-- Assign Projects to keybindings dynamically

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



-- ** Super Projects

-- Super Projects are different contexts for different projects.
-- Each Super Project has a Dynamic Project bound to S-[0..9]
-- Switching Super Project is a way to switch between a different set of applications and workspaces
-- Ex.
--  - Super Project Code:
--    Emacs | Browser | Terminals | Terminals | Terminals | Terminals | Terminals | Terminals | Terminals | Music
--  - Super Project Browsing:
--    Browser | Terminals | Terminals | Terminals | Terminals | Terminals | Keepass | Messaging | Slack | Music
data SuperProjects = SProjects [[Project]] deriving Typeable

data SPPrompt = SPPrompt
instance XPrompt SPPrompt where
    showXPrompt _ = "Super Project:"
spprompt = SPPrompt


-- Bring forth a prompt for switching Super Project
switchSuperProject = mkXPrompt spprompt def (mkComplFunFromList $ map fst defaultSuperProjectList) f
    where
      f :: String -> X()
      f name = do
        let chosenSuperProjects = filter ((==name) . fst) defaultSuperProjectList
        case chosenSuperProjects of
          [] -> return ()
          sprojects -> do
                 let chosenProject = head sprojects
                 XS.put $ AProjects $ snd chosenProject

-- mkProjectList [(i, Name)]
-- Make a list of length 10. Each Name is placed at index i in the resulting list.
-- An index that does not have a Name from xs gets a terminalsP
--
-- Note: Name must be in defaultProjectList
--
-- Ex.
-- map projectName $ mkProjectList [(3, "Emacs"), (0, "Browser")]
-- => ["Browser","Terminals","Terminals","Emacs","Terminals","Terminals","Terminals","Terminals","Terminals","Terminals"]
mkProjectList :: [(Int, ProjectName)] -> [Project]
mkProjectList xs =
    let sorted = sortWith fst xs
    in f sorted 0
      where
        f _ 10 = []
        f [] i = terminalsP : f [] (i + 1)
        f projects@((index, name):xs) i =
            if i == index
            then getP name: f xs (i+1)
            else terminalsP :f projects (i+1)
