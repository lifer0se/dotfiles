import XMonad
--  import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe (fromJust)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Named
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowNavigation

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
--  import XMonad.Util.Run
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.NamedScratchpad

import XMonad.Actions.CycleWS
--  import XMonad.Actions.UpdatePointer
import XMonad.Actions.TiledWindowDragging

import XMonad.Prelude (isNothing)

import XMonad.Layout.MultiToggle.Instances (StdTransformers (NOBORDERS))
import XMonad.Layout.MultiToggle (EOT (EOT), Toggle (Toggle), mkToggle, (??))


myTerminal :: [Char]
myTerminal = "termite"



myWorkspaces :: [[Char]]
myWorkspaces = ["  1  ","  2  ","  3  ","  4  ","  5  ","  6  ","  7  ","  8  ","  9  "]

myWorkspaceIndices :: M.Map [Char] Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable :: [Char] -> [Char]
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where
       i = fromJust $ M.lookup ws myWorkspaceIndices

------------------------------------------------------------------------
--

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "gcolor" spawnGc findGc manageGc
                , NS "galculator" spawnCalc findCalc manageCalc
                , NS "bashtop" spawnBt findBt manageBt
                , NS "calendar" spawnGsc findGsc manageGsc
                ]
  where
    spawnGc    = "gcolor2"
    findGc     = className =? "Gcolor2"
    manageGc   = customFloating $ W.RationalRect l t w h
               where
                 h = 0.3
                 w = 0.3
                 t = 0.5 -h / 2
                 l = 0.5 -w / 2
    spawnCalc  = "galculator"
    findCalc   = className =? "Galculator"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.2
                 t = 0.5 -h / 2
                 l = 0.5 -w / 2
    spawnBt    = myTerminal ++ " --class BashTOP -e bashtop"
    findBt     = className =? "BashTOP"
    manageBt   = customFloating $ W.RationalRect l t w h
               where
                 h = 0.7
                 w = 0.7
                 t = 0.52 -h / 2
                 l = 0.5 -w / 2
    spawnGsc   = "gsimplecal"
    findGsc    = className =? "Gsimplecal"
    manageGsc  = customFloating $ W.RationalRect l t w h
               where
                 h = 0.19
                 w = 0.13
                 t = 0.04
                 l = 0.5 -w / 2

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
    , ("M-q", kill)

    -- scratchpads
    , ("M-g", namedScratchpadAction myScratchPads "gcolor")
    , ("M-c", namedScratchpadAction myScratchPads "galculator")
    , ("M-y", namedScratchpadAction myScratchPads "bashtop")
    , ("M-m", namedScratchpadAction myScratchPads "calendar")

    -- spotify controls
    , ("M-<F9>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    , ("M-<F11>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    , ("M-<F12>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")

    -- volume controls
    , ("M-<Print>", spawn "amixer set Master toggle")
    , ("M-<Scroll_lock>", spawn "amixer set Master 5%-")
    , ("M-<Pause>", spawn "amixer set Master 5%+")

    -- window controls
    , ("M-h", sendMessage $ Go L)
    , ("M-j", sendMessage $ Go D)
    , ("M-k", sendMessage $ Go U)
    , ("M-l", sendMessage $ Go R)
    , ("M-S-h", sendMessage $ Swap L)
    , ("M-S-j", sendMessage $ Swap D)
    , ("M-S-k", sendMessage $ Swap U)
    , ("M-S-l", sendMessage $ Swap R)
    , ("M-C-h", sendMessage Shrink)
    , ("M-C-l", sendMessage Expand)

    , ("M-comma", sendMessage (IncMasterN 1))
    , ("M-period", sendMessage (IncMasterN (-1)))
    , ("M-<Space>", withFocused $ windows . W.sink)

    -- layout controls
    , ("M-a", sequence_ [sendMessage ToggleStruts, toggleScreenSpacingEnabled, toggleWindowSpacingEnabled])
    , ("M-S-a",sendMessage $ Toggle NOBORDERS)
    , ("M-n", sendMessage NextLayout)

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

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm .|. shiftMask, button1), dragWindow)
    , ((modm, button2), const kill)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    , ((modm, button4), const prevWS)
    , ((modm, button5), const nextWS)
    ]

------------------------------------------------------------------------
--
mySpacing :: Integer -> Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i j = spacingRaw False (Border i i i i) True (Border j j j j) True

myLayout = avoidStruts ( layoutTall ||| layoutFull)
    where
      layoutTall = named "[]=" $ windowNavigation $ draggingVisualizer $ smartBorders $ mySpacing 55 17 $ Tall 1 (3/100) (3/5)
      layoutFull = mkToggle (NOBORDERS ?? EOT) . named "[F]" $ smartBorders $ mySpacing 55 17 Full


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
    spawnOnce "picom &"
    spawnOnce "numlockx &"
    spawnOnce "setxkbmap -option caps:escape &"
    spawnOnce "nitrogen --restore &"
    spawnOnce "dunst &"
    spawnOnce "trayer --monitor 2 --edge top --align right --widthtype request --padding 6 --iconspacing 5 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x2B2E37  --height 25 --distance 5 &"
    spawnOnce "nm-applet &"
    spawnOnce "blueman-applet &"
    spawnOnce "volumeicon &"
    spawnOnce "/opt/urserver/urserver --daemon &"
    spawnOnce "play-wth-mpv &"

------------------------------------------------------------------------
--
myStatusBarSpawner :: String -> StatusBarConfig
myStatusBarSpawner n =
    statusBarProp ("xmobar -x " ++ n ++ " ~/.config/xmonad/xmobar/xmobar" ++ n ++ ".config") (pure myXmobarPP)

------------------------------------------------------------------------
--
myXmobarPP :: PP
myXmobarPP = def
    { ppSep = "   "
    , ppCurrent = xmobarColor blue "" . const wsIconFull -- . clickable
    , ppVisible = xmobarColor grey3 "" . const wsIconFull -- . clickable
    , ppVisibleNoWindows = Just (xmobarColor grey3 ""  . const wsIconFull) -- . clickable
    , ppHidden = xmobarColor grey1 "" . const wsIconFull -- . clickable
    , ppHiddenNoWindows = xmobarColor grey1 "" . const wsIconEmpty -- . clickable
    , ppUrgent = xmobarColor orange "" . const wsIconFull -- . clickable
    , ppLayout = xmobarColor grey3 "" .xmobarAction "xdotool key super+n" "1"
    , ppTitle = xmobarColor grey2 "" . xmobarAction "xdotool key super+q" "2" . shorten 80
    , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
    }
  where
    wsIconFull = " <fn=2>\xf111</fn> "
    wsIconEmpty = " <fn=2>\xf10c</fn> "
    grey1    = "#555E70"
    grey2    = "#747880"
    grey3    = "#929AAD"
    blue     = "#8BABF0"
    orange   = "#C45500"

------------------------------------------------------------------------
--
myConfig  = def
    { focusFollowsMouse  = True
    , clickJustFocuses   = False
    , borderWidth        = 3
    , modMask            = mod4Mask
    , normalBorderColor  = "#555E70"
    , focusedBorderColor = "#8BABF0"
    , terminal           = myTerminal
    , workspaces         = myWorkspaces
    , mouseBindings      = myMouseBindings
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , startupHook        = myStartupHook
    }
    `additionalKeysP` myAditionalKeys

------------------------------------------------------------------------
--

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB ( myStatusBarSpawner "0" <+> myStatusBarSpawner "1") defToggleStrutsKey
     $ myConfig
