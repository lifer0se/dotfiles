import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe (fromJust, Maybe( Just ))
import XMonad.Actions.UpdatePointer

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

import XMonad.Actions.UpdatePointer as UP
import XMonad.Actions.CycleWS

import XMonad.Layout.MultiToggle.Instances (StdTransformers (NOBORDERS))
import XMonad.Layout.MultiToggle (EOT (EOT), Toggle (Toggle), mkToggle, (??))

import XMonad.Prelude (find, findIndex, isJust, isNothing, liftM2)


myTerminal = "termite"

myWorkspaces    = ["  1  ","  2  ","  3  ","  4  ","  5  ","  6  ","  7  ","  8  ","  9  "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)
clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
    where
      i = fromJust $ M.lookup ws myWorkspaceIndices
      --  t = "<fn=2>  \xf111  </fn>" else "a"


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

    --  spotify controls
    , ("M-<F9>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    , ("M-<F11>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    , ("M-<F12>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")

    --  volume controls
    , ("M-<Print>", spawn "amixer set Master toggle")
    , ("M-<Scroll_lock>", spawn "amixer set Master 5%-")
    , ("M-<Pause>", spawn "amixer set Master 5%+")

      --
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-h", windows W.focusMaster)
    , ("M-comma", sendMessage (IncMasterN 1))
    , ("M-period", sendMessage (IncMasterN (-1)))
    , ("M-C-h", sendMessage Shrink)
    , ("M-C-l", sendMessage Expand)
    , ("M-S-l", windows W.swapDown)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-h", windows W.swapMaster)
    , ("M-<Space>", withFocused $ windows . W.sink)
    , ("M-a", sequence_ [sendMessage ToggleStruts, toggleScreenSpacingEnabled, toggleWindowSpacingEnabled])
    , ("M-S-a",sendMessage $ Toggle NOBORDERS)
    , ("M-n", sendMessage NextLayout)
    , ("M-o", nextScreen)
    , ("M-S-o", shiftNextScreen)
    , ("M-<Left>", prevWS)
    , ("M-<Right>", nextWS)
    , ("M-S-<Left>", shiftToPrev)
    , ("M-S-<Right>", shiftToNext)

    , ("M-S-q", io (exitWith ExitSuccess))
    , ("M-S-r", spawn ("killall xmobar; xmonad --recompile; xmonad --restart"))

    ]

myMouseBindings =
    [ ((4, 1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((4, 2), (\w -> focus w >> windows W.shiftMaster))
    , ((4, 3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    , ((4, 4), (\_ -> nextWS))
    , ((4, 5), (\_ -> prevWS))
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
    [ className =? "BashTOP"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , insertPosition End Newer
    ] <+> manageDocks

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
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , startupHook        = myStartupHook
    --  , logHook            = updatePointer (0.75, 0.75) (0, 0)
    }
    `additionalKeysP` myAditionalKeys `additionalMouseBindings` myMouseBindings

------------------------------------------------------------------------
--
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar -x 0 ~/.config/xmonad/xmobar/xmobar0.config" (clickablePP myXmobarPP)) defToggleStrutsKey
     . withEasySB (statusBarProp "xmobar -x 1 ~/.config/xmonad/xmobar/xmobar1.config" (clickablePP myXmobarPP)) defToggleStrutsKey
     $ myConfig
