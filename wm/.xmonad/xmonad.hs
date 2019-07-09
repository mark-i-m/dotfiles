import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, ppOutput, ppCurrent, ppVisible, ppHidden, ppHiddenNoWindows, ppTitle, ppLayout, ppSep, ppOrder, ppExtras, xmobarColor, wrap, shorten, xmobar)
import XMonad.Actions.SpawnOn
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Named
import XMonad.Operations (refresh)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docks)
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Util.NamedScratchpad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Prompt

import Control.Monad

import Data.Map (union, fromList)
import Data.Maybe (isNothing)

import System.IO (hPutStrLn)

myWorkspaces = map show [1..9]

mySP = ["NSP"]

myKeys conf@(XConfig {modMask = modm}) = fromList $
    [ ((modm, xK_z), spawn "slock")
    -- keybindings for workspaces
    , ((modm, xK_a), toggleSkip mySP)
    , ((modm, xK_s), swapNextScreen >> nextScreen)
    , ((modm, xK_Right), moveTo Next (WSIs $ allWS [notSP, hiddenWS]))
    , ((modm, xK_Left), moveTo Prev (WSIs $ allWS [notSP, hiddenWS]))
    , ((modm .|. shiftMask, xK_Right),
        shiftTo Next (WSIs notSP) >> moveTo Next (WSIs notSP))
    , ((modm .|. shiftMask, xK_Left),
        shiftTo Prev (WSIs notSP) >> moveTo Prev (WSIs notSP))
    , ((modm, xK_f), moveTo Next (WSIs $ allWS [notSP, emptyWS]))
    , ((modm .|. shiftMask, xK_f), shiftTo Next (WSIs $ allWS [notSP, emptyWS]))
    -- keybindings for stacked workspaces
    , ((modm, xK_grave), submap . fromList $
        [ ((modm, xK_grave), selectWorkspace defaultXPConfig)
        , ((modm, xK_Tab),
            withWorkspace defaultXPConfig
            (\t -> (windows . W.shift $ t) >> (windows . W.view $ t)))
        , ((modm, xK_r), removeEmptyWorkspace >>
            moveTo Next (WSIs $ allWS [notSP, hiddenWS]))
        ])
    , ((modm, xK_c), namedScratchpadAction myScratchpads "term")
    , ((0   , 0x1008FF11), spawn "amixer set Master 2-")
    , ((0   , 0x1008FF13), spawn "amixer set Master 2+")
    , ((0   , 0x1008FF12), spawn "bash ~/.xmonad/volume_mute_toggle.sh")
    , ((0   , 0x1008FFA9), spawn "touchpad-toggle")
    , ((0   , 0x1008FF02), spawn "xbacklight +10")
    , ((0   , 0x1008FF03), spawn "xbacklight -10")
    ]
    where
      toggleSkip :: [WorkspaceId] -> X ()
      toggleSkip skips = do
        hs <- gets (flip skipTags skips . W.hidden . windowset)
        unless (null hs) (windows . W.view . W.tag $ head hs)

      hiddenWS :: X (WindowSpace -> Bool)
      hiddenWS = do
        hs <- gets (map W.tag . W.hidden . windowset)
        return (\ws -> W.tag ws `elem` hs)

      emptyWS :: X (WindowSpace -> Bool)
      emptyWS = return (isNothing . W.stack)

      notSP :: X (WindowSpace -> Bool)
      notSP = return ((`notElem` mySP) . W.tag)

      allWS :: [X (WindowSpace -> Bool)] -> X (WindowSpace -> Bool)
      allWS = (liftM foldPreds) . (mapM id)
        where
          foldPreds :: [a -> Bool] -> (a -> Bool)
          foldPreds (p:tail) = (\v -> (p v) && (foldPreds tail) v)
          foldPreds [] = (\x -> True)

myScratchpads = [ NS "term" spawnTerm findTerm myFloat
    ]
    where
      spawnTerm = "gnome-terminal --hide-menubar --role=scratchpad"
      role = stringProperty "WM_WINDOW_ROLE"
      findTerm = role =? "scratchpad"
      myFloat = customFloating $ W.RationalRect l t w h
        where
          h = 0.25     -- terminal height
          w = 0.75     -- terminal width
          t = 1 - h - h/2    -- distance from top edge
          l = (1 - w)/2    -- distance from left edge

myFocusedBorderColor = "#0080FF"
myNormalBorderColor = "#753200"

myStartupHook :: X ()
myStartupHook = do
    spawnOn "1" "/home/mark/.xmonad/startup.sh"

myLogHook proc = dynamicLogWithPP $ xmobarPP
  { ppOutput  = hPutStrLn proc
  , ppSep     = " "
  , ppOrder   = (\(ws:l:t:e:_) -> [l ++ e, ws, t])
  , ppCurrent = currentStyle
  , ppVisible = visibleStyle
  , ppHidden  = hiddenStyle . noScratchPad
  , ppHiddenNoWindows = hiddenNoWinStyle . noScratchPad
  , ppTitle   = titleStyle
  , ppLayout  =  (xmobarColor "#404040" "#202020") .
                 (wrap "[" "") .
                 (\layout -> case layout of
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
    logTitles    = do
        winset <- gets windowset
        let numWins = length $ W.index winset
        let color = xmobarColor "#7a0000" "#202020"
        let sep = xmobarColor "#404040" "#202020" "]"
        return $ Just $ color $ show numWins ++ sep
    noScratchPad ws = if ws `elem` mySP then "" else ws

myLayoutHook = avoidStruts $ smartBorders $
  -- (tiled ||| Mirror tiled ||| threeCol ||| Mirror threeCol ||| Full)
  -- (tiled ||| Mirror tiled ||| latexTiled ||| Full)
  (tiled ||| Mirror tiled ||| latexTiled ||| Full)
  where
    tiled      = Tall nmaster delta ratio
    latexTiled = named "LaTeX" (Tall nmaster delta latexRatio)
    nmaster  = 1
    delta    = 3/100
    ratio    = 1/2
    latexRatio = 63/100

myManageHook = manageDocks
        <+> manageHook defaultConfig
        <+> namedScratchpadManageHook myScratchpads

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
    manageHook   = myManageHook,
    layoutHook   = myLayoutHook,

    -- window border color
    focusedBorderColor = myFocusedBorderColor,
    normalBorderColor = myNormalBorderColor
    }

main = do
    xmobar <- spawnPipe "xmobar"
    xmonad $ docks $ myConfig xmobar
