import XMonad
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map as M
--  import XMonad.Prelude
import Data.Maybe (fromJust)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Named
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.MultiToggle (EOT (EOT), Toggle (Toggle), mkToggle, (??))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Util.ClickableWorkspaces
import XMonad.Actions.CycleWS
import XMonad.Actions.TiledWindowDragging
import qualified XMonad.Actions.FlexibleResize as Flex


myTerminal :: [Char]
myTerminal = "termite"

colors :: String -> String
colors c
    | c == "grey1"  = "#2B2E37"
    | c == "grey2"  = "#555E70"
    | c == "grey3"  = "#747880"
    | c == "grey4"  = "#929AAD"
    | c == "blue"   = "#8BABF0"
    | c == "orange" = "#C45500"
    | otherwise = ""


myWorkspaces :: [[Char]]
myWorkspaces = ["  1  ","  2  ","  3  ","  4  ","  5  ","  6  ","  7  ","  8  ","  9  "]

myWorkspaceIndices :: M.Map [Char] Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable :: [Char] -> [Char]
clickable ws = "<action=xdotool key super+"++show i++">" ++ ws ++ "</action>"
    where
       i = fromJust $ M.lookup ws myWorkspaceIndices
       wsIcon = if True then " <fn=2>\xf111</fn>  " else " <fn=2>\xf10c</fn>  "


------------------------------------------------------------------------
--

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "gcolor" "gcolor2" (className =? "Gcolor2") defaultFloating
                , NS "galculator" "galculator" (className =? "Galculator") (customFloating $ W.RationalRect 0.4 0.25 0.2 0.5)
                , NS "htop" (myTerminal ++ " --class htop -e htop") (className =? "htop") (customFloating $ W.RationalRect 0.17 0.15 0.7 0.7)
                , NS "calendar" "gsimplecal" (className =? "Gsimplecal") (customFloating $ W.RationalRect 0.435 0.04 0.13 0.19)
                , NS "brightness" "brightness-controller" (title =? "Brightness Controller") defaultFloating
                ]


------------------------------------------------------------------------
--

myAditionalKeys :: [(String, X ())]
myAditionalKeys =

    -- apps
    [ ("M-<Return>", spawn myTerminal)
    , ("M-v", spawn (myTerminal ++ " -e nvim"))
    , ("M-f", spawn (myTerminal ++ " -e ranger"))
    , ("M-d", spawn "dmenu_run")
    , ("M-p", spawn "passmenu")
    , ("M-w", spawn "brave")
    , ("M-S-w", spawn "brave --incognito")
    , ("M-S-f", spawn "pcmanfm")
    , ("M-s", spawn "spotify")
    , ("<Print>", spawn "flameshot gui")
    , ("M-q", kill)

    -- scratchpads
    , ("M-g", namedScratchpadAction myScratchPads "gcolor")
    , ("M-c", namedScratchpadAction myScratchPads "galculator")
    , ("M-y", namedScratchpadAction myScratchPads "htop")
    , ("M-r", namedScratchpadAction myScratchPads "calendar")
    , ("M-b", namedScratchpadAction myScratchPads "brightness")

    -- spotify controls
    , ("M-<F9>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    , ("M-<F11>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    , ("M-<F12>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")

    -- volume controls
    , ("M-<Print>", spawn "amixer set Master toggle")
    , ("M-<Scroll_lock>", spawn "amixer set Master 5%-")
    , ("M-<Pause>", spawn "amixer set Master 5%+")

    -- window controls
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-h", windows W.focusMaster)
    , ("M-l", windows W.focusDown)
    , ("M-S-l", windows W.swapDown)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-h", windows W.swapMaster)
    , ("M-C-h", sendMessage Shrink)
    , ("M-C-l", sendMessage Expand)
    , ("M-C-j", sendMessage MirrorShrink)
    , ("M-C-k", sendMessage MirrorExpand)
    , ("M-comma", sendMessage (IncMasterN 1))
    , ("M-period", sendMessage (IncMasterN (-1)))
    , ("M-<Space>", withFocused $ windows . W.sink)

    -- layout controls
    , ("M-a", sendMessage $ Toggle NBFULL)
    , ("M-n", sendMessage NextLayout)
    , ("M-m", spawn "xdotool key super+a && xdotool key super+b")

    -- workspace controls
    , ("M-<Left>", prevWS)
    , ("M-<Right>", nextWS)

    -- screen controll
    , ("M-o", nextScreen)
    , ("M-S-o", shiftNextScreen)
    , ("M-S-<Left>", shiftToPrev)
    , ("M-S-<Right>", shiftToNext)

    -- kill / restart xmonad
    , ("M-S-q", io exitSuccess)
    , ("M-S-r", spawn "killall xmobar; xmonad --recompile; xmonad --restart")

    ]


myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm .|. shiftMask, button1), dragWindow)
    , ((modm, button2), const kill)
    , ((modm, button3), \w -> focus w >> Flex.mouseResizeWindow w)
    , ((modm, button4), const prevWS)
    , ((modm, button5), const nextWS)
    ]


------------------------------------------------------------------------
--

mySpacing :: Integer -> Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i j = spacingRaw False (Border i i i i) True (Border j j j j) True

myLayout = avoidStruts ( layoutTall ||| layoutTabbed )
    where
      layoutTall = mkToggle (NBFULL ?? EOT) . named "[]=" $ draggingVisualizer $ smartBorders $ mySpacing 55 15 $ ResizableTall 1 (3/100) (3/5) []
      layoutTabbed = mkToggle (NBFULL ?? EOT) . named "[ t ]" $ smartBorders $ mySpacing 55 15 $ tabbed shrinkText myTabTheme
      myTabTheme = def { fontName      = "xft:Roboto:size=12:bold"
                 , activeColor         = colors "grey1"
                 , inactiveColor       = colors "grey1"
                 , activeBorderColor   = colors "grey1"
                 , inactiveBorderColor = colors "grey1"
                 , activeTextColor     = colors "blue"
                 , inactiveTextColor   = colors "grey3"
                 , decoHeight          = 25
                 }


------------------------------------------------------------------------
--

myManageHook :: ManageHook
myManageHook = composeAll
    [ insertPosition End Newer
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ] <+> manageDocks <+> namedScratchpadManageHook myScratchPads


------------------------------------------------------------------------
--

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "lxsession"
    spawnOnce "numlockx &"
    spawnOnce "setxkbmap -option caps:escape &"
    spawnOnce "nitrogen --restore &"
    spawnOnce "dunst &"
    spawnOnce "picom &"
    spawnOnce "trayer --monitor 2 --edge top --align right --widthtype request --padding 10 --iconspacing 5 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x2B2E37  --height 25 --distance 5 &"
    spawnOnce "nm-applet &"
    spawnOnce "blueman-applet &"
    spawnOnce "volumeicon &"
    spawnOnce "/opt/urserver/urserver --daemon &"
    spawnOnce "play-wth-mpv &"


------------------------------------------------------------------------
--

myStatusBarSpawner :: String -> StatusBarConfig
myStatusBarSpawner n =
    statusBarProp ("xmobar -x " ++ n ++ " ~/.config/xmonad/xmobar/xmobar" ++ n ++ ".config") (clickablePP $ filterOutWsPP [ scratchpadWorkspaceTag ] myXmobarPP)


------------------------------------------------------------------------
--

myXmobarPP :: PP
myXmobarPP = def
    { ppSep = "     "
    , ppCurrent = xmobarColor (colors "blue") "" . xmobarBorder borderPosition (colors "blue") borderHeight
    , ppVisible = xmobarColor (colors "grey4") "" . xmobarBorder borderPosition (colors "grey4") borderHeight
    , ppVisibleNoWindows = Just (xmobarColor (colors "grey4") "")
    , ppHidden = xmobarColor (colors "grey2") "" . xmobarBorder borderPosition (colors "grey2") borderHeight
    , ppHiddenNoWindows = xmobarColor (colors "grey2") ""
    , ppUrgent = xmobarColor (colors "orange") "" . xmobarBorder borderPosition (colors "orange") borderHeight
    , ppLayout = xmobarColor (colors "grey4") "" . xmobarAction "xdotool key super+n" "1"
    , ppTitle = xmobarColor (colors "grey3") "" . xmobarAction "xdotool key super+q" "2" . shorten 90
    , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
    }
    where
      borderHeight = 3
      borderPosition= "Bottom"


------------------------------------------------------------------------
--

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withSB ( myStatusBarSpawner "0" <+> myStatusBarSpawner "1")
     $ def
        { focusFollowsMouse  = True
        , clickJustFocuses   = False
        , borderWidth        = 3
        , modMask            = mod4Mask
        , normalBorderColor  = colors "grey2"
        , focusedBorderColor = colors "blue"
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        , mouseBindings      = myMouseBindings
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , handleEventHook    = docksEventHook
        , startupHook        = myStartupHook
        }
        `additionalKeysP` myAditionalKeys
