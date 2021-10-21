import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Named

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.NamedScratchpad

import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer

import XMonad.Layout.MultiToggle.Instances (StdTransformers (NOBORDERS))
import XMonad.Layout.MultiToggle (EOT (EOT), Toggle (Toggle), mkToggle, (??))


myTerminal = "termite"
myWorkspaces = ["  1  ","  2  ","  3  ","  4  ","  5  ","  6  ","  7  ","  8  ","  9  "]

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
    [ ("M-<Return>", spawn (myTerminal))
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
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-h", windows W.focusMaster)
    , ("M-C-h", sendMessage Shrink)
    , ("M-C-l", sendMessage Expand)
    , ("M-S-l", windows W.swapDown)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-h", windows W.swapMaster)
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
    , ("M-S-q", io (exitWith ExitSuccess))
    , ("M-S-r", spawn ("killall xmobar; xmonad --recompile; xmonad --restart"))

    ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    , ((modm, button4), (\_ -> prevWS))
    , ((modm, button5), (\_ -> nextWS))
    ]

------------------------------------------------------------------------
--
mySpacing i j = spacingRaw False (Border i i i i) True (Border j j j j) True
myLayout = avoidStruts ( layoutTall ||| layoutFull)
    where
      layoutTall = named "[]=" $ smartBorders $ mySpacing 65 15 $ Tall 1 (3/100) (3/5)
      layoutFull = mkToggle (NOBORDERS ?? EOT) . named "[F]" $ smartBorders $ mySpacing 65 15 $ Full


------------------------------------------------------------------------
--
myManageHook = composeAll
    [ insertPosition End Newer
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ] <+> manageDocks <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------
--
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
myXmobarPP :: PP
myXmobarPP = def
    { ppSep = "   "
    , ppCurrent = xmobarColor blue "" . xmobarBorder position blue 2
    , ppVisible = xmobarColor grey3 "" . xmobarBorder position grey3 2
    , ppVisibleNoWindows = Just (xmobarColor grey3 "" . xmobarBorder position grey3 2)
    , ppHidden = xmobarColor grey1 "" . xmobarBorder position grey1 2
    , ppHiddenNoWindows = xmobarColor grey1 ""
    , ppUrgent = xmobarColor orange "" . xmobarBorder position orange 2
    , ppTitle = xmobarColor grey2 "" . shorten 60
    , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
    }
  where
    position, grey1, grey2, grey3, blue, orange :: String
    position = "Bottom"
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
     . withEasySB (statusBarProp "xmobar -x 0 ~/.config/xmonad/xmobar/xmobar0.config" (clickablePP myXmobarPP)) defToggleStrutsKey
     . withEasySB (statusBarProp "xmobar -x 1 ~/.config/xmonad/xmobar/xmobar1.config" (clickablePP myXmobarPP)) defToggleStrutsKey
     $ myConfig
