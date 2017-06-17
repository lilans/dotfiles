Config { font = "xft:Source Code Pro Regular:pixelsize=10:antialias=true"
  , bgColor = "#161E25"
  , fgColor = "#657b83"
  , border = NoBorder
  , borderColor = "black"
  , hideOnStart = False
  , persistent = True
  , position = Top
  , lowerOnStart = True
  , commands = [ Run Weather "UUEE" ["-t"," <tempC>C","-L","64","-H","77","--normal","#657b83","--high","#657b83","--low","#657b83"] 36000
  , Run Network "wlp3s0" ["-L","0","-H","32","--normal","#657b83","--high","#657b83"] 10
  , Run Cpu ["-L","3","-H","50","--normal","#657b83","--high","#657b83"] 10
  , Run Memory ["-t","Mem: <usedratio>%"] 10
  , Run Com "~/.xmonad/volume.sh" [] "vol" 1
  , Run Date "<fc=#93a1a1>%a %b %_d %Y %H:%M</fc>" "date" 10
  , Run StdinReader
]
  , sepChar = "%"
  , alignSep = "}{"
  , template = " %StdinReader% }{ %cpu% | %memory% | %wlp3s0% | spb:%UUEE% | %date% "
}
