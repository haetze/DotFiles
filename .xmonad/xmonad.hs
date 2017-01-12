import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
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
        [ --((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
          --, ((0, xK_Print), spawn "scrot")
          ((mod1Mask, xK_o), spawn "xtrlock")
        , ((mod1Mask, xK_s), spawn "scrot '%Y-%m-%d_%H:%M:%S.png' -e 'mv $f ~/shots/'" )
        , ((mod1Mask, xK_a), spawn "scrot -s '%Y-%m-%d_%H:%M:%S.png' -e 'mv $f ~/shots/'" )
        ]
