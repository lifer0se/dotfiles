import XMonad
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map as M
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
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Tabbed

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Util.Loggers (logLayoutOnScreen, logTitleOnScreen, shortenL, wrapL)

import XMonad.Actions.CycleWS
import XMonad.Actions.TiledWindowDragging
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.WindowBringer


myTerminal :: [Char]
myTerminal = "termite"

grey1, grey2, grey3, grey4, blue, orange :: String
grey1  = "#2B2E37"
grey2  = "#555E70"
grey3  = "#747880"
grey4  = "#8691A8"
blue   = "#8BABF0"
orange = "#C45500"

myWorkspaces :: [[Char]]
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

actionPrefix, actionButton, actionSuffix :: [Char]
actionPrefix = "<action=`xdotool key super+"
actionButton = "` button="
actionSuffix = "</action>"

addActions :: [(String, Int)] -> String -> String
addActions [] ws = ws
addActions (x:xs) ws = addActions xs (actionPrefix ++ k ++ actionButton ++ show b ++ ">" ++ ws ++ actionSuffix)
    where k = fst x
          b = snd x


------------------------------------------------------------------------
--

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "gcolor" "gcolor2" (className =? "Gcolor2") (customFloating $ W.RationalRect 0.35 0.35 0.3 0.3)
  , NS "galculator" "galculator" (className =? "Galculator") (customFloating $ W.RationalRect 0.4 0.25 0.2 0.5)
  , NS "htop" (myTerminal ++ " --class htop -e htop") (className =? "htop") (customFloating $ W.RationalRect 0.17 0.15 0.7 0.7)
  , NS "calendar" "gsimplecal" (className =? "Gsimplecal") (customFloating $ W.RationalRect 0.435 0.04 0.13 0.19)
  , NS "brightness" "brightness-controller" (title =? "Brightness Controller") defaultFloating
  ]


------------------------------------------------------------------------
--

nonNSP :: WSType
nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

myAditionalKeys :: [(String, X ())]
myAditionalKeys =

    -- apps
  [ ("M-<Return>", spawn myTerminal)
  , ("M-v", spawn $ myTerminal ++ " -e nvim")
  , ("M-f", spawn $ myTerminal ++ " -e ranger")
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
  , ("M-C-j", sendMessage ShrinkSlave)
  , ("M-C-k", sendMessage ExpandSlave)
  , ("M-comma", sendMessage $ IncMasterN 1)
  , ("M-period", sendMessage $ IncMasterN (-1))
  , ("M-<Space>", withFocused $ windows . W.sink)
  , ("M-t", gotoMenu' "dmenu")

  -- layout controls
  , ("M-a", sendMessage $ Toggle NBFULL)
  , ("M-n", sendMessage NextLayout)
  , ("M-m", spawn "xdotool key super+a && xdotool key super+b")

  -- workspace controls
  , ("M-<Left>", moveTo Prev nonNSP)
  , ("M-<Right>", moveTo Next nonNSP)

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
  , ((modm, button4), \_ -> moveTo Prev nonNSP)
  , ((modm, button5), \_ -> moveTo Next nonNSP)
  ]


------------------------------------------------------------------------
--

mySpacing :: Integer -> Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i j = spacingRaw False (Border i i i i) True (Border j j j j) True

myLayout = avoidStruts $ layoutTall ||| layoutTabbed
  where
    layoutTall = mkToggle (NBFULL ?? EOT) . named "[]=" $ draggingVisualizer $ smartBorders $ mySpacing 55 15 $ mouseResizableTile { masterFrac = 0.65, draggerType = FixedDragger 0 30}
    layoutTabbed = mkToggle (NBFULL ?? EOT) . named "[ t ]" $ smartBorders $ mySpacing 55 15 $ tabbed shrinkText myTabTheme
    myTabTheme = def
      { fontName            = "xft:Roboto:size=12:bold"
      , activeColor         = grey1
      , inactiveColor       = grey1
      , activeBorderColor   = grey1
      , inactiveBorderColor = grey1
      , activeTextColor     = blue
      , inactiveTextColor   = grey3
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


------------------------------------------------------------------------
--

myWorkspaceIndices :: M.Map [Char] Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..]

clickable :: [Char] -> [Char] -> [Char]
clickable ic ws  = addActions [ (show i, 1), ("q", 2), ("Left", 4), ("Right", 5) ] ic
                    where i = fromJust $ M.lookup ws myWorkspaceIndices

xmobar0, xmobar1 :: StatusBarConfig
xmobar0 = statusBarPropTo "xmobar0" "xmobar -x 0 ~/.config/xmonad/xmobar/xmobar0.config" (myXmobarPP 0)
xmobar1 = statusBarPropTo "xmobar1" "xmobar -x 1 ~/.config/xmonad/xmobar/xmobar1.config" (myXmobarPP 1)

myStatusBarSpawner :: ScreenId -> IO StatusBarConfig
myStatusBarSpawner 0 = pure xmobar0
myStatusBarSpawner 1 = pure xmobar1
myStatusBarSpawner _ = mempty

myXmobarPP :: ScreenId -> X PP
myXmobarPP s = pure $ filterOutWsPP [ scratchpadWorkspaceTag ] $ def
  { ppSep = "     "
  , ppCurrent = xmobarColor blue "" . clickable wsIconFull
  , ppVisible = xmobarColor grey4 "" . clickable wsIconFull
  , ppVisibleNoWindows = Just (xmobarColor grey4 "" . clickable wsIconEmpty)
  , ppHidden = xmobarColor grey2 "" . clickable wsIconFull
  , ppHiddenNoWindows = xmobarColor grey2 "" . clickable wsIconEmpty
  , ppUrgent = xmobarColor orange "" . clickable wsIconFull
  , ppLayout = xmobarColor grey4 ""
  , ppTitle = xmobarColor grey3 ""
  , ppOrder = \(ws : _ : _ : extras) -> ws : extras
  , ppExtras  = [ wrapL (actionPrefix ++ "n" ++ actionButton ++ "1>") actionSuffix $ logLayoutOnScreen s
                , wrapL (actionPrefix ++ "q" ++ actionButton ++ "2>") actionSuffix $ shortenL 80 $ logTitleOnScreen s
                ]
  }
  where
    wsIconFull = "  <fn=2>\xf111</fn>  "
    wsIconEmpty = "  <fn=2>\xf10c</fn>  "


------------------------------------------------------------------------
--

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . dynamicSBs myStatusBarSpawner
     $ def
       { focusFollowsMouse  = True
       , clickJustFocuses   = False
       , borderWidth        = 3
       , modMask            = mod4Mask
       , normalBorderColor  = grey2
       , focusedBorderColor = blue
       , terminal           = myTerminal
       , workspaces         = myWorkspaces
       , mouseBindings      = myMouseBindings
       , layoutHook         = myLayout
       , manageHook         = myManageHook
       , handleEventHook    = docksEventHook
       , startupHook        = myStartupHook
       }
       `additionalKeysP` myAditionalKeys
