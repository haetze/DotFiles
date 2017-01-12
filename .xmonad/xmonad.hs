import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe, runInTerm)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


main = do
    xmproc <- spawnPipe "/home/haetze/.cabal/bin/xmobar /home/haetze/.xmonad/xmobar.hs"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \s-> return ()
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        } `additionalKeys`
        [ ((mod1Mask, xK_o), spawn "xtrlock")
        , ((mod1Mask, xK_s), spawn "scrot '%Y-%m-%d_%H:%M:%S.png' -e 'mv $f ~/shots/'" )
        , ((mod1Mask, xK_F1), spawn "/home/haetze/usefulCommands/1live.sh")
        , ((mod1Mask, xK_F2), spawn "pkill mpg123")
        , ((mod1Mask, xK_F12), spawn "sysctl hw.snd.default_unit=1")
        , ((mod1Mask, xK_F11), spawn "sysctl hw.snd.default_unit=0")
        ]
