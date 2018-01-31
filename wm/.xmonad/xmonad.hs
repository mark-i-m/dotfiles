import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, ppOutput, ppCurrent, ppVisible, ppHidden, ppHiddenNoWindows, ppTitle, ppLayout, ppSep, ppOrder, ppExtras, xmobarColor, wrap, shorten, xmobar)
import XMonad.Actions.SpawnOn
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Named
import XMonad.Layout.Groups (group)
import XMonad.Layout.Groups.Helpers (focusUp, focusDown, swapUp, swapDown, swapMaster, moveToGroupUp, moveToGroupDown, focusGroupDown)
--import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Operations (refresh)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap

import Data.Map (union, fromList)

import System.IO (hPutStrLn)

myWorkspaces = map show [1..9]

myKeys conf@(XConfig {modMask = modm}) = fromList $
    [ ((modm, xK_z), spawn "slock")
    , ((modm, xK_q), spawn "bash ~/.xmonad/restart.sh")
    
    -- keybindings for workspaces
    , ((modm, xK_Right), nextWS)
    , ((modm, xK_Left), prevWS)
    , ((modm .|. shiftMask, xK_Right), shiftToNext >> nextWS)
    , ((modm .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
    , ((modm, xK_f), moveTo Next EmptyWS)
    , ((modm .|. shiftMask, xK_f), shiftTo Next EmptyWS)
    -- keybindings within workspaces
    , ((modm, xK_Tab), focusDown)
    , ((modm .|. shiftMask, xK_Tab), focusUp)
    , ((modm, xK_j), focusDown)
    , ((modm, xK_k), focusUp)
    , ((modm .|. shiftMask, xK_j), swapDown >> refresh)
    , ((modm .|. shiftMask, xK_k), swapUp >> refresh)
    , ((modm, xK_Return), swapMaster >> refresh)
    -- keybindings for stacked workspaces
    , ((modm, xK_grave), submap . fromList $
        [ ((modm, xK_grave), focusGroupDown)
        , ((modm, xK_Down), moveToGroupDown False)
        , ((modm, xK_Up), moveToGroupUp False)
        ])
    
    , ((0   , 0x1008FF11), spawn "amixer set Master 2-")
    , ((0   , 0x1008FF13), spawn "amixer set Master 2+")
    , ((0   , 0x1008FF12), spawn "bash ~/.xmonad/volume_mute_toggle.sh")
    , ((0   , 0x1008FFA9), spawn "touchpad-toggle")
    , ((0   , 0x1008FF02), spawn "xbacklight +10")
    , ((0   , 0x1008FF03), spawn "xbacklight -10")
    ]

myFocusedBorderColor = "#0080FF"
myNormalBorderColor = "#000000"

myStartupHook :: X ()
myStartupHook = do
    spawnOn "1" "/home/mark/.xmonad/startup.sh"

myLogHook proc = dynamicLogWithPP $ xmobarPP
  { ppOutput  = hPutStrLn proc
  , ppSep     = " "
  , ppOrder   = (\(ws:l:t:e:_) -> [l ++ e, ws, t])
  , ppCurrent = currentStyle
  , ppVisible = visibleStyle
  , ppHidden  = hiddenStyle
  , ppHiddenNoWindows = hiddenNoWinStyle
  , ppTitle   = titleStyle
  , ppLayout  =  (xmobarColor "#404040" "#202020") . 
                 (wrap "[" "") . 
                 (\layout -> case dropByFull layout of
                  "Tall"        -> "|"
                  "Mirror Tall" -> "-"
                  "LaTeX"       -> "L"
                  -- "ThreeCol"    -> "3Col"
                  -- "Mirror ThreeCol" -> "Mirror 3Col"
                  "Full"        -> "#"
                  x -> "Error parsing (see xmonad.hs): " ++ x
                  ) 
  , ppExtras  = [logTitles]
  }
  where
    currentStyle = xmobarColor "yellow" ""
    visibleStyle = xmobarColor "#717700" ""
    hiddenStyle  = xmobarColor "grey" ""
    hiddenNoWinStyle  = xmobarColor "#202020" ""
    titleStyle   = xmobarColor "#008000" "" . shorten 130 . filterCurly
    filterCurly  = filter (not . isCurly)
    isCurly x    = x == '{' || x == '}'
    dropByFull   = reverse . (drop 8) . reverse -- drop the chars " by Full"
    logTitles    = do
        winset <- gets windowset
        let numWins = length $ W.index winset
        let color = xmobarColor "#7a0000" "#202020"
        let sep = xmobarColor "#404040" "#202020" "]"
        return $ Just $ color $ show numWins ++ sep 
        
myLayoutHook = avoidStruts $ smartBorders $
  -- (tiled ||| Mirror tiled ||| threeCol ||| Mirror threeCol ||| Full)
  -- (tiled ||| Mirror tiled ||| latexTiled ||| Full)
  group (tiled ||| Mirror tiled ||| latexTiled ||| Full) Full
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
