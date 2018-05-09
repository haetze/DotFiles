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
       , font = "xft:Bitstream Vera Sans Mono:size=18:bold:anitalias=true"
       , alignSep = "}{"
       
       , commands = [Run Com "/home/haetze/usefulCommands/.battery" [] "battery" 10
                    ,Run Com "/home/haetze/usefulCommands/.audio" [] "audio" 10
                    ,Run Com "/home/haetze/usefulCommands/.snd" [] "sound" 10
                    ,Run Com "cat" ["/home/haetze/usefulCommands/.radio"] "radio" 10
           ]
       , template = "<fc=#ee9a00>%date%</fc>| %uname% | %battery% | %sound% on %audio% | %radio%"
       }
