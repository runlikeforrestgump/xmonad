import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce
import System.IO (Handle)

-- My Colours
dzenBGcolour :: String
dzenBGcolour = "black"
dzenFGcolour :: String
dzenFGcolour = "white"

ppCurrentBGcolour :: String
ppCurrentBGcolour = dzenBGcolour
ppCurrentFGcolour :: String
ppCurrentFGcolour = "green"

ppHiddenBGcolour :: String
ppHiddenBGcolour = dzenBGcolour
ppHiddenFGcolour :: String
ppHiddenFGcolour = "blue"

ppHiddenNoWindowsBGcolour :: String
ppHiddenNoWindowsBGcolour = dzenBGcolour
ppHiddenNoWindowsFGcolour :: String
ppHiddenNoWindowsFGcolour = "yellow"

ppTitleBGcolour :: String
ppTitleBGcolour = dzenBGcolour
ppTitleFGcolour :: String
ppTitleFGcolour = dzenFGcolour

ppUrgentBGcolour :: String
ppUrgentBGcolour = dzenBGcolour
ppUrgentFGcolour :: String
ppUrgentFGcolour = "red"

-- My Defaults
myBorderWidth :: Dimension
myBorderWidth = 0

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
    [ ((myModMask, xK_p), shellPrompt myPromptConf)
    , ((mod1Mask, xK_Tab), windows W.focusDown)
    , ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusUp)
    ]

myLayoutHook = avoidStruts $ noBorders $ minimize $ tiled ||| Mirror tiled ||| Full
    where
        tiled = ResizableTall nmaster delta ratio []
        nmaster = 1
        delta = 3/100
        ratio = 1/2

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent = dzenColor ppCurrentFGcolour ppCurrentBGcolour
    , ppHidden = dzenColor ppHiddenFGcolour ppHiddenBGcolour
    , ppHiddenNoWindows = dzenColor ppHiddenNoWindowsFGcolour ppHiddenNoWindowsBGcolour
    , ppOrder = \(ws:_:t:_) -> [ws, t]
    , ppOutput = hPutStrLn h
    , ppSep = " | "
    , ppTitle = dzenColor ppTitleFGcolour ppTitleBGcolour
    , ppUrgent = dzenColor ppUrgentFGcolour ppUrgentBGcolour
    , ppWsSep = " "
    }

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "trayer" --> doIgnore ]

myModMask :: KeyMask
myModMask = mod4Mask

myStartupHook :: [String]
myStartupHook = [ "nitrogen --set-scaled ~/.wallpapers/Current"
                , "nm-applet &"
                , "urxvtd -q -o -f"
                , "xcompmgr &"
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
    , font = "-*-ubuntu mono-medium-r-normal-*-11-*-*-*-*-*-*-*" 
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
    , font' = Nothing
    , lineHeight = Just 23
    , width = Just 1065
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

-- My trayer
data TrayerConf = TrayerConf
    { alpha :: Maybe Int
    , distance :: Maybe Int
    , distanceFrom :: Maybe String
    , iconAlignment :: Maybe String
    , height' :: Maybe Int 
    , screenEdge :: Maybe String
    , setPartialStrut :: Maybe Bool
    , tint :: Maybe String
    , transparent :: Maybe Bool
    , widthType :: Maybe String
    }

myTrayerConf :: TrayerConf
myTrayerConf = TrayerConf
    { alpha = Just 0
    , distance = Just 225
    , distanceFrom = Just "right"
    , iconAlignment = Just "right"
    , height' = Just 23
    , screenEdge = Just "top"
    , setPartialStrut = Just True
    , tint = Just "0x000000"
    , transparent = Just True
    , widthType = Just "request"
    }

trayer :: TrayerConf -> String
trayer conf = unwords $ ["trayer"]
    ++ addArg ("--align", fmap show $ iconAlignment conf)
    ++ addArg ("--alpha", fmap show $ alpha conf)
    ++ addArg ("--distance", fmap show $ distance conf)
    ++ addArg ("--distancefrom", fmap show $ distanceFrom conf)
    ++ addArg ("--edge", fmap show $ screenEdge conf)
    ++ addArg ("--height", fmap show $ height' conf)
    ++ addArg ("--SetPartialStrut", fmap show $ setPartialStrut conf)
    ++ addArg ("--tint", fmap show $ tint conf)
    ++ addArg ("--transparent", fmap show $ transparent conf)
    ++ addArg ("--widthtype", fmap show $ widthType conf)
  where
    addArg (_, Nothing) = []
    addArg (opt, Just val) = [opt, val]

-- Main
main = do
    myDzenBar <- spawnPipe $ dzen2 myDzenConf
    spawnPipe $ conkyDzen myConkyrc myDzenConf {
        width = Just 215
        , xPosition = Just 1065
        }
    spawnPipe $ trayer myTrayerConf

    xmonad $ defaultConfig
        { borderWidth = myBorderWidth
        , focusFollowsMouse = myFocusFollowsMouse
        , keys = myKeys <+> keys defaultConfig
        , layoutHook = myLayoutHook
        , logHook = myLogHook myDzenBar >> fadeInactiveLogHook 0.75
        , manageHook = myManageHook
        , modMask = myModMask
        , startupHook = mapM_ spawnOnce myStartupHook
        , terminal = myTerminal
        , workspaces = myWorkspaces
        }
