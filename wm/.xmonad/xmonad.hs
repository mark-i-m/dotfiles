import XMonad
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.Script
import qualified Data.Map as M

myWorkspaces = map show [1..9]

myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_z), spawn "slock")
    , ((modm, xK_q), spawn "bash ~/.xmonad/restart.sh")
    ]

myFocusedBorderColor = "#0080FF"
myNormalBorderColor = "#000000"

myConfig = defaultConfig {
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
