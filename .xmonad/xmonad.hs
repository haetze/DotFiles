import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Core
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe, runInTerm)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


main = do
    xmproc <- spawnPipe "/home/haetze/.cabal/bin/xmobar /home/haetze/.xmonad/xmobar.hs"
    xmonad $ docks $ def
        { manageHook = manageDocks <+> manageHook def
        , layoutHook = avoidStruts  $  layoutHook def
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \s-> return ()
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        } `additionalKeys`
        [ ((mod1Mask, xK_o), spawn "xtrlock")
        , ((mod1Mask, xK_s), spawn "scrot '%Y-%m-%d_%H:%M:%S.png' -e 'mv $f ~/shots/'" )
        , ((mod1Mask, xK_u), spawn "xterm -fa 'Monospace' -fs 12x24" )
        , ((mod1Mask, xK_f)
          , spawn "find ~/ | sed 's/ /\\ /g' | sort -f | dmenu -i -l 20 -nb '#2e3436' -nf '#9999CC' | /home/haetze/usefulCommands/openTerminal.sh" )
        , ((mod1Mask, xK_F1), spawn "/home/haetze/usefulCommands/1live.sh")
        , ((mod1Mask, xK_F2), spawn "/home/haetze/usefulCommands/pkill.sh mpg123 /home/haetze/usefulCommands/.radio")
        , ((mod1Mask, xK_F12), spawn "sysctl hw.snd.default_unit=6")
        , ((mod1Mask, xK_F11), spawn "sysctl hw.snd.default_unit=4")
        , ((mod1Mask, xK_F9), spawn "amixer set Master 1%-")
        , ((mod1Mask, xK_F10), spawn "amixer set Master 1%+")
        ]
