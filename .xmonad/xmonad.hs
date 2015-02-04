import XMonad
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.Script
import qualified Data.Map as M

myWorkspaces = map show [1..8] ++ ["email"]

myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_z), spawn "slock")
    , ((0    , 0x1008FF11), spawn "amixer set Master 2-")
    , ((0    , 0x1008FF13), spawn "amixer set Master 2+")
    , ((0    , 0x1008FF12), spawn "~/.xmonad/volume_mute_toggle.sh")
    , ((0    , 0x1008FFA9), spawn "touchpad-toggle")
    , ((0    , 0x1008FF02), spawn "xbacklight +10")
    , ((0    , 0x1008FF03), spawn "xbacklight -10")
    , ((modm , xK_q      ), spawn "killall trayer nm-applet ; xmonad --recompile && xmonad --restart")
    ]

myFocusedBorderColor = "#0080FF"
myNormalBorderColor = "#000000"

myConfig = defaultConfig {
    -- automount, desktop background, systray
    startupHook = execScriptHook "'/home/mark/.xmonad/nautilus_trayer.sh'",

    -- terminal
    terminal = "/usr/bin/gnome-terminal",

    -- workspace names
    workspaces  = myWorkspaces,

    -- keyBindings
    modMask     = mod4Mask, -- use the windows key for mod key
    keys        = \c -> myKeys c `M.union` keys defaultConfig c,

    -- window border color
    focusedBorderColor = myFocusedBorderColor,
    normalBorderColor = myNormalBorderColor
    }

main = xmonad =<< xmobar myConfig
