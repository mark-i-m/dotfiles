import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, ppOutput, ppCurrent, ppVisible, ppHidden, ppHiddenNoWindows, ppTitle, ppLayout, ppSep, ppOrder, xmobarColor, wrap, shorten, xmobar)
import XMonad.Actions.SpawnOn
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Named
--import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS

import Data.Map (union, fromList)

import System.IO (hPutStrLn)

myWorkspaces = map show [1..9] ++ (map snd myExtraWorkspaces)

myExtraWorkspaces = [(xK_0, "E"), (xK_minus, "_")]

myKeys conf@(XConfig {modMask = modm}) = fromList $
    [ ((modm, xK_z), spawn "slock")
    , ((modm, xK_q), spawn "bash ~/.xmonad/restart.sh")
    , ((modm, xK_Right), nextWS)
    , ((modm, xK_Left), prevWS)
    , ((modm .|. shiftMask, xK_Right), shiftToNext >> nextWS)
    , ((modm .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
    , ((modm, xK_f), moveTo Next EmptyWS)
    , ((modm .|. shiftMask, xK_f), shiftTo Next EmptyWS)
    , ((0   , 0x1008FF11), spawn "amixer set Master 2-")
    , ((0   , 0x1008FF13), spawn "amixer set Master 2+")
    , ((0   , 0x1008FF12), spawn "bash ~/.xmonad/volume_mute_toggle.sh")
    , ((0   , 0x1008FFA9), spawn "touchpad-toggle")
    , ((0   , 0x1008FF02), spawn "xbacklight +10")
    , ((0   , 0x1008FF03), spawn "xbacklight -10")
    ] 
    ++ [((modm, key), (windows $ W.greedyView ws)) | (key, ws) <- myExtraWorkspaces]
    ++ [((modm .|. shiftMask, key), (windows $ W.shift ws)) | (key, ws) <- myExtraWorkspaces]

myFocusedBorderColor = "#0080FF"
myNormalBorderColor = "#000000"

myStartupHook :: X ()
myStartupHook = do
    spawnOn "1" "/home/mark/.xmonad/startup.sh"

myLogHook proc = dynamicLogWithPP $ xmobarPP
  { ppOutput  = hPutStrLn proc
  , ppSep     = " "
  , ppOrder   = (\(ws:l:t:_) -> [l, ws,t])
  , ppCurrent = currentStyle
  , ppVisible = visibleStyle
  , ppHidden  = hiddenStyle
  , ppHiddenNoWindows = hiddenNoWinStyle
  , ppTitle   = titleStyle
  , ppLayout  =  (xmobarColor "#404040" "#202020") . (wrap "[" "]") . (\layout -> case layout of
      "Tall"        -> "|"
      "Mirror Tall" -> "-"
      "LaTeX" -> "L"
      -- "ThreeCol"    -> "3Col"
      -- "Mirror ThreeCol" -> "Mirror 3Col"
      "Full"        -> "#"
      )
  }
  where
    currentStyle = xmobarColor "yellow" ""
    visibleStyle = xmobarColor "#717700" ""
    hiddenStyle  = xmobarColor "grey" ""
    hiddenNoWinStyle  = xmobarColor "#202020" ""
    titleStyle   = xmobarColor "#008000" "" . shorten 130 . filterCurly
    filterCurly  = filter (not . isCurly)
    isCurly x    = x == '{' || x == '}'

myLayoutHook = avoidStruts $ smartBorders $ 
  -- (tiled ||| Mirror tiled ||| threeCol ||| Mirror threeCol ||| Full)
  (tiled ||| Mirror tiled ||| latexTiled ||| Full)
  where
    tiled      = Tall nmaster delta ratio
    latexTiled = named "LaTeX" (Tall nmaster delta latexRatio)
    -- threeCol = ThreeColMid nmaster delta ratio
    nmaster  = 1
    delta    = 3/100
    ratio    = 1/2
    latexRatio = 63/100

myConfig logHandle = defaultConfig {
    -- automount, desktop background, systray
    startupHook = myStartupHook,

    -- terminal
    terminal = "/usr/bin/gnome-terminal",

    -- workspace names
    workspaces  = myWorkspaces,

    -- keyBindings
    modMask     = mod4Mask, -- use the windows key for mod key
    keys        = \c -> myKeys c `union` keys defaultConfig c,

    -- xmobar
    logHook      = myLogHook logHandle,
    layoutHook   = myLayoutHook,
    manageHook   = manageDocks,

    -- window border color
    focusedBorderColor = myFocusedBorderColor,
    normalBorderColor = myNormalBorderColor
    }

main = do
    xmobar <- spawnPipe "xmobar"
    xmonad $ myConfig xmobar
