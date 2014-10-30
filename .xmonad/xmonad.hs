
import XMonad
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.Script
import qualified Data.Map as M

myWorkspaces = map show [1..8] ++ ["email"]

myKeys (XConfig {modMask = modm}) = M.fromList $
    [  ((mod4Mask, xK_l), spawn "slock")
    , ((0    , 0x1008FF11), spawn "amixer set Master 2-")
    , ((0    , 0x1008FF13), spawn "amixer set Master 2+")
    , ((0    , 0x1008FF12), spawn "amixer -D pulse set Master 1+ toggle")
    , ((0    , 0x1008FFA9), spawn "touchpad-toggle")
    , ((modm , xK_q      ), spawn "killall trayer nm-applet ; xmonad --recompile && xmonad --restart")
    ]

myConfig = defaultConfig {
    -- automount, desktop background, systray
    startupHook = execScriptHook "'/home/mark/.xmonad/nautilus_trayer.sh'",
    
    -- workspace names
    workspaces  = myWorkspaces,

    -- keyBindings
    keys        = \c -> myKeys c `M.union` keys defaultConfig c
    }

main = xmonad =<< xmobar myConfig
