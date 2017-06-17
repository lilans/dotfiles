import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- Terminal

myTerminal = "termite"

------------------------------------------------------------------------
-- Workspaces

myWorkspaces = ["1:term", "2:web", "3:messenger", "4:dev", "5:emacs", "6:read", "7", "8", "9", "10"]

------------------------------------------------------------------------
-- Window rules

myManageHook = composeAll . concat $
  [[className =? c --> doF (W.shift "1:term")       | c <- myTerm]
  , [className =? c --> doF (W.shift "2:web")       | c <- myWeb]
  , [className =? c --> doF (W.shift "3:messenger") | c <- myMessenger]
  , [className =? c --> doF (W.shift "4:dev")       | c <- myDev]
  , [className =? c --> doF (W.shift "5:emacs")     | c <- myEmacs]
  , [className =? c --> doF (W.shift "6:read")      | c <- myRead]
  , [manageDocks]
  ] where
    myTerm = ["termite"]
    myWeb = ["chromium", "opera"]
    myMessenger = ["telegram"]
    myDev = ["qtcreator", "pycharm"]
    myEmacs = ["Emacs"]
    myRead = ["evince", "calibre"]

------------------------------------------------------------------------
-- Layouts

myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

------------------------------------------------------------------------
-- Colors and borders

myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.

tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#34B67A"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#F56F6C"

-- Width of the window border in pixels.
myBorderWidth = 1

------------------------------------------------------------------------
-- Main

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
  xmonad $ defaults {
      logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = "   "
      }
      , manageHook = manageDocks <+> myManageHook
      , startupHook = setWMName "LG3D"
  }

defaults = defaultConfig {
  terminal           = myTerminal
  , borderWidth        = myBorderWidth
  , modMask            = mod4Mask
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , layoutHook         = smartBorders $ myLayout
  , manageHook         = myManageHook
  , startupHook        = setWMName "LG3D"
}`additionalKeys` myKeys
myKeys = [
           ((mod4Mask, xK_p), spawn "rofi -show run")
         , ((mod4Mask, xK_q), spawn "sflock")
         , ((mod4Mask, xK_o), spawn "pcmanfm")
         , ((mod4Mask, xK_b), sendMessage ToggleStruts)
         ]
