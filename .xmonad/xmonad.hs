import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Script
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import System.IO

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 120
                        }
        , startupHook = execScriptHook "'~/.xmonad/nautilus_trayer.sh'" -- for automount and background
        } `additionalKeys`
           [  ((mod4Mask, xK_l), spawn "slock")
            , ((0                     , 0x1008FF11), spawn "amixer set Master 2-")
            , ((0                     , 0x1008FF13), spawn "amixer set Master 2+")
            , ((0                     , 0x1008FF12), spawn "amixer -D pulse set Master 1+ toggle")
            , ((0                     , 0x1008FFA9), spawn "touchpad-toggle")
           ]
