Config { font = "xft:Roboto:size=12:bold"
       , additionalFonts =
          [ "xft:FontAwesome 5 Free Solid:pixelsize=14"
          , "xft:FontAwesome:pixelsize=10:bold"
          , "xft:FontAwesome 5 Free Solid:pixelsize=16"
          , "xft:Hack Nerd Font Mono:pixelsize=21"
          , "xft:Hack Nerd Font Mono:pixelsize=25"
          ]
       , border = NoBorder
       , bgColor = "#2B2E37"
       , fgColor = "#929AAD"
       , alpha = 255
       , position = TopSize L 100 40
       , textOffset = 24
       , textOffsets = [ 25, 24 ]
       , lowerOnStart = True
       , allDesktops = True
       , persistent = False
       , hideOnStart = False
       , iconRoot = ".config/xmonad/xmobar/icons/"
       , commands =
         [ Run UnsafeXPropertyLog "_XMONAD_LOG_0"
         , Run Date "%a, %d %b   <fn=1></fn>   %H:%M:%S" "date" 10
         , Run Memory ["-t","Mem: <fc=#AAC0F0><usedratio></fc>%"] 10
         , Run Com "/home/amnesia/.config/xmonad/xmobar/cpu_temp.sh" [] "cpu" 10
         , Run Com "/home/amnesia/.config/xmonad/xmobar/gpu_temp.sh" [] "gpu" 10
         , Run Com "/home/amnesia/.config/xmonad/xmobar/available_updates.sh" [] "updates" 10
         , Run Com "/home/amnesia/.config/xmonad/xmobar/volume.sh" [] "volume" 10
         , Run Com "/home/amnesia/.config/xmonad/xmobar/bluetooth.sh" [] "bluetooth" 10
         , Run Com "/home/amnesia/.config/xmonad/xmobar/wifi.sh" [] "network" 10
         ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "\
            \<action=`xdotool key super+Right` button=5><action=`xdotool key super+Left` button=4><action=xdotool key super+d>      <icon=menu.xpm/>      </action></action></action>\
            \%_XMONAD_LOG_0% } \
            \<action=xdotool key super+r>%date%</action> { \
            \<action=setsid -f $TERMINAL -e ~/scripts/pop_upgrade.sh>%updates%</action>\
            \<action=xdotool key super+y>     %cpu%     |     %memory%     |     %gpu%</action>        \
            \<action=`exec blueman-manager` button=1>   %bluetooth%   </action> \
            \%network%\
            \<action=`volume.sh up` button=4><action=`volume.sh down` button=5><action=`volume.sh mute` button=1><action=`exec pavucontrol` button=3>    %volume%       </action></action></action></action>"
       }
