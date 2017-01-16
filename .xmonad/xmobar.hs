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
       , alignSep = "}{"
       
       , commands = [
           Run Com "/home/haetze/.battery" [] "battery" 60,
           Run Com "/home/haetze/.battery2" [] "state" 60,
           Run Com "/home/haetze/.network" [] "network" 60,
           Run Com "/home/haetze/.mem.sh" [] "mem" 60,
           Run Com "/home/haetze/.audio" [] "audio" 60
           ]
       , template = "<fc=#ee9a00>%date%</fc>| %uname% | %battery% | %state% | %network% | %audio% | %mem% |"
       }
