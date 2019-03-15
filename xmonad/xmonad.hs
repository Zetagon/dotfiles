-- * Imports
{-# LANGUAGE DeriveDataTypeable #-}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP, removeKeysP)
import XMonad.Util.SpawnOnce
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
import System.Process
import Control.Monad
import XMonad.Prompt
import XMonad.Prompt.Shell
import GHC.Exts (sortWith)
import XMonad.Config.Kde

-- * Entry point
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/leo/.xmobarrc" -- Start xmobar with config
  xmonad $ dynamicProjects projects -- Launch dynamic projects
         $ kdeConfig
             { manageHook = manageHook kdeConfig <+> myManageHook
             , startupHook = myStartupHook
             , layoutHook = myLayouts
             , handleEventHook = myEvents
             , logHook = myLog xmproc
             , modMask = mod4Mask}
               `additionalKeysP`
               myKeymap
               `removeKeysP` ["M4-p"] -- Reserved for rofi using xbindkeys
-- * StartupHook
myStartupHook = sequence_ [spawnOnce s | s <- startupList]
startupList =
  [ "sleep 5 && for i in `xdotool search --all --name xmobar`; do xdotool windowraise $i; done"
  ]
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
myKeymap = ([ ("M4-/", dmenuSwitchProjectPrompt)
            , ("M4-S-<Return>", spawn "xfce4-terminal")
            , ("M4-C-/", shiftToProjectPrompt def)
            , ("M4-d", XS.put $ AProjects defaultProjectList)
            , ("M4-s", switchProjectContext)
            , ("M4-x", spawn "for i in `xdotool search --all --name xmobar`; do xdotool windowraise $i; done") --bring xmobar to front
            , ("M4-f", spawn "~/dotfiles/keyboard.sh")
            , ("M4-m", namedScratchpadAction scratchpads "htop")
            , ("M4-S-x", namedScratchpadAction scratchpads "capture")
            , ("M4-g", namedScratchpadAction scratchpads "agenda")
            , ("M4-c", namedScratchpadAction scratchpads "cmus")]
               ++ map (\x -> ("M4-S-" ++ show x, shiftToProjectNr x)) [0..9] -- Move window to project nr x
               ++ map (\x-> ("M4-" ++ show x, goToProjectNr x)) [0..9]) -- Assign a project to position x
-- * Scratchpads

scratchpads =
-- run htop in xterm, find it by title, use default floating window placement
    [ NS "htop" "xterm -e htop" (title =? "htop") defaultFloating
    , NS "cmus" "xterm -xrm 'XTerm*vt100.allowTitleOps: false'   -T \"cmus\" -e cmus" (title =? "cmus") defaultFloating
    , NS "capture" "org-capture" (title =? "org-capture")
      (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "agenda" "emacsclient -c -F '((name . \"org-agenda\"))' -e '(org-agenda nil \"a\")'"
          (title =? "org-agenda")
          (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))]
-- * Project list
projects = [ makeEmacsProject "Emacs" "~/" ""
           , makeEmacsProject "XMonadConfig" "~/.xmonad" "~/.xmonad/xmonad.hs ~/.xmobarrc "
           , makeSimpleProject "XMonad Browser" ["firefox - --new-window"]
           , makeEmacsProject "Spirited Away" "~/Documents/texter/analys-spirited-away" "~/Documents/texter/analys-spirited-away/master.tex"
           , makeSimpleProject "Spirited Away Browser" ["firejail --noprofile firefox - --new-window"]
           , makeSimpleProject "Watch" ["crunchyroll"]
           , makeSimpleProject "Keepass" ["keepassx"]
           , makeSimpleProject "Mail" ["thunderbird"]
           , makeSimpleProject "Terminals" $ replicate 3 "xfce4-terminal"
           , makeSimpleProject "Browser" ["firefox"]
           , makeSimpleProject "Messaging" [ "firefox --new-window https://discordapp.com/login"
                                           , "firefox --new-window https://perpetuality.slack.com/"]
           , makeSimpleProject "VLC" ["vlc"]
           , makeSimpleProject "Mpsyt" ["xterm -e \"firejail --noprofile mpsyt\""]
           , makeSimpleProject "Zotero" ["zotero"]

           , makeEmacsProject "SchoolEmacs" "~/Documents/dvkand" "~/Documents/dvkand/"
           , makeSimpleProject "SchoolBrowser" ["firefox --new-window"]

           , makeEmacsProject "SekreterareEmacs" "~/Documents/styrelsemöten" "~/Documents/styrelsemöten/"
           , makeSimpleProject "SekreterareSlack" ["firefox --new-window https://styrelsedv.slack.com/"]
           , makeSimpleProject "SekreterareBrowser" ["firefox --new-window"]
           , makeSimpleProject "1" []
           , makeSimpleProject "2" []
           , makeSimpleProject "3" []
           , makeSimpleProject "4" []
           , makeSimpleProject "5" []
           , makeSimpleProject "6" []
           , makeSimpleProject "7" []
           , makeSimpleProject "8" []
           , makeSimpleProject "9" []
           , makeSimpleProject "0" []
           ]
numberP n = getP $ show n
terminalsP = getP "Terminals"




defaultProjectList = mkProjectList [ (1, "Emacs")
                                   , (2, "Browser")
                                   , (5, "Mail")
                                   , (8, "Keepass")
                                   , (9, "Messaging")
                                   , (0, "Mpsyt")]
-- * Project Context List
defaultProjectContextList = [ ("default" , defaultProjectList)
                          , ("watch", mkProjectList [(1, "Watch"), (8, "Keepass")])
                          , ("spirited away", mkProjectList [(1, "Spirited Away"), (2, "Spirited Away Browser"), (3, "VLC"), (7, "Zotero"), (0, "Mpsyt")])
                          , ("xmonad", mkProjectList [ (1, "XMonadConfig")
                                                     , (2, "XMonad Browser")
                                                     , (8, "Keepass")
                                                     , (0, "Mpsyt")])
                          , ("browser", mkProjectList [(2, "Browser")])
                          , ("school", mkProjectList [ (1, "SchoolEmacs")
                                                     , (2, "SchoolBrowser")
                                                     , (5, "Mail")
                                                     , (8, "Keepass")])
                          , ("sekreterare", mkProjectList [ (1, "SekreterareEmacs")
                                                          , (2, "SekreterareBrowser")
                                                          , (8, "Keepass")
                                                          , (5, "Mail")
                                                          , (9, "SekreterareSlack")])
                          , ("numbered", mkProjectList $ map (\n -> (n, show n)) [0..9])
                          ]
-- * Code
-- ** Dynamic Project utility functions
makeEmacsProject name path files =
    Project
      { projectName = name
      , projectDirectory = path
      , projectStartHook = Just $ do
                             spawn $ "emacsclient -c " ++ files
                             spawn "xfce4-terminal"
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

dmenuSwitchProjectPrompt = do
  wspaceName <- io $ dmenu $ map projectName projects
  project <- lookupProject wspaceName
  case project of
    Nothing -> return ()
    Just project -> switchProject project

-- ** Active Projects
-- TODO Make a better name for this
-- Assign Projects to keybindings dynamically

data ActiveProjects = AProjects [Project] deriving Typeable
instance ExtensionClass ActiveProjects where
    initialValue = AProjects $ defaultProjectList



goToProjectNr n = do
  AProjects projects <- XS.get
  switchProject $ projects !! n

shiftToProjectNr n = do
  AProjects projects <- XS.get
  shiftToProject $ projects !! n

switchActiveProjectNr n = do
    switchProjectPrompt def
    project <- currentProject
    AProjects activeProjects <- XS.get :: X ActiveProjects
    XS.put $ AProjects $ switch n project activeProjects

switch :: Int -> a -> [a] -> [a]
switch n x xs = (take n xs) ++ x:(drop (n + 1) xs)



-- ** Project Contexts

-- Project Context are different contexts for different projects.
-- Each Project Context has a Dynamic Project bound to S-[0..9]
-- Switching Project Context is a way to switch between a different set of applications and workspaces
-- Ex.
--  - Project Context Code:
--    Emacs | Browser | Terminals | Terminals | Terminals | Terminals | Terminals | Terminals | Terminals | Music
--  - Project Context Browsing:
--    Browser | Terminals | Terminals | Terminals | Terminals | Terminals | Keepass | Messaging | Slack | Music
data ProjectContexts = PContexts [[Project]] deriving Typeable

data SPPrompt = SPPrompt
instance XPrompt SPPrompt where
    showXPrompt _ = "Project Context:"
spprompt = SPPrompt


-- Bring forth a prompt for switching Project Context
-- switchProjectContext = mkXPrompt spprompt def (mkComplFunFromList $ map fst defaultProjectContextList) f
switchProjectContext = do
  pName <- io $ dmenu (map fst defaultProjectContextList)
  f pName
    where
      f :: String -> X()
      f name = do
        let chosenProjectContext = filter ((==name) . fst) defaultProjectContextList
        case chosenProjectContext of
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
        f [] i = (numberP i) : f [] (i + 1)
        f projects@((index, name):xs) i =
            if i == index
            then getP name: f xs (i+1)
            else (numberP i):f projects (i+1)--terminalsP :f projects (i+1)

dmenu :: [String] -> IO(String)
dmenu xs =
    do
      (_, Just hout, _, _) <- createProcess (shell ( "echo -e \"" ++ (tail $ concatMap ('\n':) xs) ++ "\" | dmenu")){std_out = CreatePipe}
      hGetLine hout
