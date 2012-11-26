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
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import System.IO (Handle)

-- My Colours
dzenBGcolour :: String
dzenBGcolour = "black"
dzenFGcolour :: String
dzenFGcolour = "white"

ppCurrentBGcolour :: String
ppCurrentBGcolour = dzenBGcolour
ppCurrentFGcolour :: String
ppCurrentFGcolour = "white"

ppHiddenBGcolour :: String
ppHiddenBGcolour = dzenBGcolour
ppHiddenFGcolour :: String
ppHiddenFGcolour = "yellow"

ppHiddenNoWindowsBGcolour :: String
ppHiddenNoWindowsBGcolour = dzenBGcolour
ppHiddenNoWindowsFGcolour :: String
ppHiddenNoWindowsFGcolour = "gray"

ppTitleBGcolour :: String
ppTitleBGcolour = dzenBGcolour
ppTitleFGcolour :: String
ppTitleFGcolour = dzenFGcolour

ppUrgentBGcolour :: String
ppUrgentBGcolour = dzenBGcolour
ppUrgentFGcolour :: String
ppUrgentFGcolour = "red"

-- My Defaults
-- See XMonad/Config.hs for configuration options.
myBorderWidth :: Dimension
myBorderWidth = 0

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

--myLayoutHook = avoidStruts $ noBorders $ minimize $ windowSwitcherDecoration $ draggingVisualizer $ tiled ||| Mirror tiled ||| Full
myLayoutHook = avoidStruts $ noBorders $ tiled ||| Mirror tiled ||| Full
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

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "urxvtc"

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..4]

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
    , font :: Maybe String
    , lineHeight :: Maybe Int
    , width :: Maybe Int
    , xPosition :: Maybe Int
    , yPosition :: Maybe Int
    }

defaultDzenConf :: DzenConf
defaultDzenConf = DzenConf
    { alignment = Just LeftAlign
    , bgColour = Just dzenBGcolour
    , fgColour = Just dzenFGcolour
    , font = Nothing
    , lineHeight = Just 15
    , width = Nothing
    , xPosition = Just 0
    , yPosition = Just 0
    } 

dzen2 :: DzenConf -> String
dzen2 c = unwords $ ["dzen2"]
    ++ addArg ("-ta", fmap show $ alignment c)
    ++ addArg ("-bg", fmap quote $ bgColour c)
    ++ addArg ("-fg", fmap quote $ fgColour c)
    ++ addArg ("-fn", fmap quote $ font c)
    ++ addArg ("-h",  fmap show $ lineHeight c)
    ++ addArg ("-w",  fmap show $ width c)
    ++ addArg ("-x",  fmap show $ xPosition c)
    ++ addArg ("-y",  fmap show $ yPosition c)
  where
    quote = ("'" ++ ) . ( ++ "'")
    addArg (_, Nothing) = []
    addArg (opt, Just val) = [opt, val]

-- My Conky

-- My trayer

main = do
    -- spawnPipe $ conkyDzen
    myDzenBar <- spawnPipe $ dzen2 defaultDzenConf
    -- spawnPipe $ tray
    xmonad $ defaultConfig
        { borderWidth = myBorderWidth
        , focusFollowsMouse = myFocusFollowsMouse
        , layoutHook = myLayoutHook
        , logHook = myLogHook myDzenBar >> fadeInactiveLogHook 1.0
        , modMask = myModMask
        , terminal = myTerminal
        , workspaces = myWorkspaces
        }
