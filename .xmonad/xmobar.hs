Config { borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , alpha = 255
       , position = Top
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , sepChar = "%"
       , additionalFonts = []
       , font = "xft:Bitstream Vera Sans Mono:size=9:bold:anitalias=true"
       , alignSep = "}{"
       
       , commands = [
            Run Memory [ "-t"       , "Mem: <usedratio>%"
                       , "--low"    , "darkgreen"
                       , "--normal" , "darkorange"
                       , "--high"   , "darkred"
                       ] 10
           ,Run Swap [] 10
           ,Run Cpu [ "-L"       , "3"
                    , "-H"       , "50"
                    , "--normal" , "green"
                    , "--high"   , "red"
                   ] 10
           ,Run Battery [ "--template" , "Batt: <acstatus>"
                        , "--low"      , "darkred"
                        , "--normal"   , "darkorange"
                        , "--high"     , "darkgreen"
                        , "--"
                        , "-o"         , "<left>% (<timeleft>)"
                        , "-O"         , "<fc=#dAA520>Charging</fc>"
                        , "-i"         , "<fc=#006000>Charged</fc>"
                        ] 10
           ,Run DynNetwork [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                           , "--low"      , "darkgreen"
                           , "--normal"   , "darkorange"
                           , "--high"     , "darkred"
                           ] 10
           ,Run Weather "EDLW" [ "--template" , "<skyCondition> | <fc=#4682B4><tempC></fc>C"
                               ] 3600
           ]
       , template = "<fc=#ee9a00>%date%</fc>| %uname% | %memory% | %dynnetwork% | %cpu% | %battery% | %EDLW% "
       }
