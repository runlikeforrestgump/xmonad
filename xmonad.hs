import XMonad
import XMonad.Core
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Util.Run (spawnPipe)

-- My Defaults
-- See XMonad/Config.hs for configuration options.
myBorderWidth :: Dimension
myBorderWidth = 0

-- myFocusedBorderColor :: String
-- myFocusedBorderColor

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- myHandleEventHook :: Event -> X All
-- myHandleEventHook =

-- myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
-- myKeys = 

myLayoutHook = avoidStruts $ noBorders $ windowSwitcherDecoration $ draggingVisualizer $ tiled ||| Mirror tiled ||| Full ||| 
    where
        tiled = ResizableTall nmaster delta ratio
        nmaster = 1
        delta = 3/100
        ratio = 1/2

-- myLogHook :: X ()
-- myLogHook = 

-- myManageHook :: ManageHook
-- myManageHook = 

myModMask :: KeyMask
myModMask = mod4Mask

-- myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
-- myMouseBindings = 

-- myNormalBorderColor :: String
-- myNormalBorderColor = 

-- myStartupHook :: X ()
-- myStartupHook = 

myTerminal :: String
myTerminal = "urxvtc"

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..4]

main = do
    -- spawnPipe $ conkyDzen
    -- spawnPipe $ dzen
    -- spawnPipe $ tray
    xmonad $ defaultConfig
        { borderWidth = myBorderWidth
        , focusFollowsMouse = myFocusFollowsMouse
        , layoutHook = myLayoutHook
        , modMask = myModMask
        , terminal = myTerminal
        , workspaces = myWorkspaces
        }
