import Data.Char (chr)
import Data.List (isPrefixOf)
import Graphics.X11.ExtraTypes
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.UrgencyHook (clearUrgents, focusUrgent, NoUrgencyHook(NoUrgencyHook), withUrgencyHook)
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce
import System.Environment (getEnv)
import System.IO (Handle)

-- My Colours
dzenBGcolour :: String
dzenBGcolour = "#fdf6e3"
dzenFGcolour :: String
dzenFGcolour = "#000000"

ppCurrentBGcolour :: String
ppCurrentBGcolour = "#268bd2"
ppCurrentFGcolour :: String
ppCurrentFGcolour = "white"

ppHiddenBGcolour :: String
ppHiddenBGcolour = dzenBGcolour
ppHiddenFGcolour :: String
ppHiddenFGcolour = "blue"

ppHiddenNoWindowsBGcolour :: String
ppHiddenNoWindowsBGcolour = dzenBGcolour
ppHiddenNoWindowsFGcolour :: String
ppHiddenNoWindowsFGcolour = "#b58900"

ppTitleBGcolour :: String
ppTitleBGcolour = dzenBGcolour
ppTitleFGcolour :: String
ppTitleFGcolour = dzenFGcolour

ppUrgentBGcolour :: String
ppUrgentBGcolour = "#dc322f"
ppUrgentFGcolour :: String
ppUrgentFGcolour = "white"

-- My Defaults
myBorderWidth :: Dimension
myBorderWidth = 0

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

data KeyCommands = KeyCommands
    { firefox :: String
    , nextSong :: String
    , nowPlayingSong :: String
    , playPauseSong :: String
    , previousSong :: String
    , restartXmonad :: String
    , stopMusic :: String
    , volumeDown :: String
    , volumeToggle :: String
    , volumeUp :: String
    }

myKeyCommands :: KeyCommands
myKeyCommands = KeyCommands
    { firefox = "/usr/local/bin/firefox/firefox &"
    , nextSong = "notify-send \"$(ncmpcpp next --now-playing '%a - \"%t\"')\""
    , nowPlayingSong = "notify-send \"$(ncmpcpp --now-playing '%a - \"%t\"')\""
    , playPauseSong = "ncmpcpp toggle"
    , previousSong = "notify-send \"$(ncmpcpp prev --now-playing '%a - \"%t\"')\""
    , restartXmonad = "killall conky dzen2; xmonad --recompile; xmonad --restart"
    , stopMusic = "ncmpcpp stop"
    , volumeDown = "amixer -q set Master 1- unmute"
    , volumeToggle = "amixer -q set Master toggle"
    , volumeUp = "amixer -q set Master 1+ unmute"
    }

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
    [ ((0, xF86XK_AudioLowerVolume), spawn (volumeDown myKeyCommands))
    , ((0, xF86XK_AudioRaiseVolume), spawn (volumeUp myKeyCommands))
    , ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusUp)
    , ((mod1Mask, xK_Tab), windows W.focusDown)
    , ((myModMask .|. shiftMask, xK_BackSpace), clearUrgents >> refresh)
    , ((myModMask .|. shiftMask, xK_z), spawn (nowPlayingSong myKeyCommands))
    , ((myModMask, xK_a), spawn (previousSong myKeyCommands))
    , ((myModMask, xK_b), spawn (volumeToggle myKeyCommands))
    , ((myModMask, xK_BackSpace), focusUrgent)
    , ((myModMask, xK_f), spawn (firefox myKeyCommands))
    , ((myModMask, xK_p), shellPrompt myPromptConf)
    , ((myModMask, xK_q), spawn (restartXmonad myKeyCommands))
    , ((myModMask, xK_s), spawn (nextSong myKeyCommands))
    , ((myModMask, xK_x), spawn (stopMusic myKeyCommands))
    , ((myModMask, xK_z), spawn (playPauseSong myKeyCommands))
    ]

myLayoutHook = avoidStruts $ noBorders $ minimize $ tiled ||| Mirror tiled ||| Full
    where
        tiled = ResizableTall nmaster delta ratio []
        nmaster = 1
        delta = 3/100
        ratio = 1/2

myLogHook :: Handle -> String -> X ()
myLogHook h plusIcon = dynamicLogWithPP $ defaultPP
    { ppCurrent = dzenColor ppCurrentFGcolour ppCurrentBGcolour . wrap plusIcon " "
    , ppHidden = dzenColor ppHiddenFGcolour ppHiddenBGcolour . wrap plusIcon " "
    , ppHiddenNoWindows = dzenColor ppHiddenNoWindowsFGcolour ppHiddenNoWindowsBGcolour . wrap plusIcon " "
    , ppOrder = \(ws:_:t:_) -> [ws, t]
    , ppOutput = hPutStrLn h
    , ppSep = " " ++ [(chr 183)] ++ "  "
    , ppTitle = dzenColor ppTitleFGcolour ppTitleBGcolour
    , ppUrgent = dzenColor ppUrgentFGcolour ppUrgentBGcolour . wrap plusIcon " "
    , ppWsSep = " "
    }

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Chromium" --> doShift "2"
    , className =? "Xfce4-notifyd" --> doIgnore
    ]

myModMask :: KeyMask
myModMask = mod4Mask

myStartupHook :: [String]
myStartupHook = [ "nitrogen --set-scaled ~/.wallpapers/Current"
                , "nm-applet &"
                , "urxvtd -q -o -f"
                , "xcompmgr &"
                , "xrdb ~/.Xresources"
                , "xscreensaver -no-splash &"
                ]

myTerminal :: String
myTerminal = "urxvtc"

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..4]

-- My xmonad prompt (better than dmenu)
myPromptConf :: XPConfig 
myPromptConf = defaultXPConfig 
    { autoComplete = Nothing 
    , bgColor = "#333333" 
    , bgHLight = "#4422EE" 
    , borderColor = "#333333" 
    , completionKey = xK_Tab 
    , defaultText = "" 
    , fgColor = "#EEEEEE" 
    , fgHLight = "#333333" 
    , font = "-misc-inconsolata-medium-*-*-*-12-*-*-*-*-*-*-*"
    , height = 23
    , historyFilter = id
    , historySize = 23
    , position = Top 
    , promptBorderWidth = 0 
    , promptKeymap = defaultXPKeymap 
    , searchPredicate = isPrefixOf 
    , showCompletionOnTab = True 
    }

-- My dzen2
data TextAlignDzen = LeftAlign | RightAlign | Centered
instance Show TextAlignDzen where
    show Centered   = "c"
    show LeftAlign  = "l"
    show RightAlign = "r"

data DzenConf = DzenConf
    { alignment :: Maybe TextAlignDzen
    , bgColour :: Maybe String
    , fgColour :: Maybe String
    , font' :: Maybe String
    , lineHeight :: Maybe Int
    , width :: Maybe Int
    , xPosition :: Maybe Int
    , yPosition :: Maybe Int
    }

myDzenConf :: DzenConf
myDzenConf = DzenConf
    { alignment = Just LeftAlign
    , bgColour = Just dzenBGcolour
    , fgColour = Just dzenFGcolour
    , font' = Just "xft:Inconsolata:size=12:style=Regular"
    , lineHeight = Just 23
    , width = Just 905
    , xPosition = Just 0
    , yPosition = Just 0
    } 

dzen2 :: DzenConf -> String
dzen2 conf = unwords $ ["dzen2"]
    ++ addArg ("-ta", fmap show $ alignment conf)
    ++ addArg ("-bg", fmap quote $ bgColour conf)
    ++ addArg ("-fg", fmap quote $ fgColour conf)
    ++ addArg ("-fn", fmap quote $ font' conf)
    ++ addArg ("-h",  fmap show $ lineHeight conf)
    ++ addArg ("-w",  fmap show $ width conf)
    ++ addArg ("-x",  fmap show $ xPosition conf)
    ++ addArg ("-y",  fmap show $ yPosition conf)
  where
    quote = ("'" ++ ) . ( ++ "'")
    addArg (_, Nothing) = []
    addArg (opt, Just val) = [opt, val]

-- My Conky
myConkyrc :: String
myConkyrc = "~/.xmonad/conkyrc"

conkyDzen :: String -> DzenConf -> String
conkyDzen "" _ = ""
conkyDzen conkyrc dzenConfig = conky ++ " | " ++ (dzen2 dzenConfig)
  where
    conky = "conky -c " ++ conkyrc

-- Main
main = do
    plusIcon <- fmap ("^i(" ++) (fmap (++ "/.xmonad/icons/thayer/plus.xbm)") (getEnv "HOME"))
    myDzenBar <- spawnPipe $ dzen2 myDzenConf
    spawnPipe $ conkyDzen myConkyrc myDzenConf
        { alignment = Just RightAlign 
        , width = Just 375
        , xPosition = Just 905 
        }

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { borderWidth = myBorderWidth
        , focusFollowsMouse = myFocusFollowsMouse
        , keys = myKeys <+> keys defaultConfig
        , layoutHook = myLayoutHook
        , logHook = myLogHook myDzenBar plusIcon >> fadeInactiveLogHook 0.75
        , manageHook = myManageHook
        , modMask = myModMask
        , startupHook = mapM_ spawnOnce myStartupHook
        , terminal = myTerminal
        , workspaces = myWorkspaces
        }
