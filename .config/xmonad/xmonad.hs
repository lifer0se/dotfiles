{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import XMonad
import System.Exit
import Prelude hiding (log)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Semigroup
import Data.Bits (testBit)
import Control.Monad (unless, when)
import Foreign.C (CInt)
import Data.Foldable (find)

import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position (Master, End))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.StatusBar
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat, doSink)
import XMonad.Hooks.RefocusLast (isFloat)

import XMonad.Layout.Spacing (Spacing, spacingRaw, Border (Border))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Named (named)
import XMonad.Layout.Decoration (ModifiedLayout)
import XMonad.Layout.DraggingVisualizer (draggingVisualizer)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.MultiToggle (EOT (EOT), Toggle (Toggle), mkToggle, (??))
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.IndependentScreens
import XMonad.Layout.HintedGrid
import XMonad.Layout.PerWorkspace

import XMonad.Util.NamedScratchpad
import XMonad.Util.Loggers (logLayoutOnScreen, logTitleOnScreen, shortenL, wrapL, xmobarColorL)
import XMonad.Util.EZConfig (additionalKeysP)
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Actions.CycleWS
import XMonad.Actions.TiledWindowDragging
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.OnScreen (onlyOnScreen)
import XMonad.Actions.Warp (warpToScreen)
import Data.List
import qualified Data.List as L


myTerminal, myTerminalClass :: [Char]
myTerminal = "alacritty"
myTerminalClass = "Alacritty"

grey1, grey2, grey3, grey4, cyan, orange :: String
grey1  = "#2B2E37"
grey2  = "#555E70"
grey3  = "#697180"
grey4  = "#8691A8"
cyan   = "#8BABF0"
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
  [ NS "gcolor" "gcolor2" (className =? "Gcolor2") defaultFloating
  , NS "galculator" "galculator" (className =? "Galculator") (customFloating $ W.RationalRect 0.58 0.48 0.2 0.4)
  , NS "bashtop" (myTerminal ++ " --title bashtop -e bashtop") (title =? "bashtop") (customFloating $ W.RationalRect 0.17 0.15 0.7 0.7)
  , NS "calendar" "gsimplecal" (className =? "Gsimplecal") (customFloating $ W.RationalRect 0.435 0.05 0.13 0.21)
  , NS "brightness" "brightness-controller" (title =? "Brightness Controller") defaultFloating
  , NS "caprine" "caprine" (className =? "Caprine") defaultFloating
  ]


------------------------------------------------------------------------
--

currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)

workspaceOnCurrentScreen :: WSType
workspaceOnCurrentScreen = WSIs $ do
  s <- currentScreen
  return $ \x -> W.tag x /= "NSP" && isOnScreen s x

myAditionalKeys :: [(String, X ())]
myAditionalKeys =

    -- apps
  [ ("M-<Return>", spawn myTerminal)
  , ("M-S-<Return>", spawn "open_term_at_dir.sh")
  , ("M-v", spawn $ myTerminal ++ " --title Nvim -e nvim")
  , ("M-f", spawn $ myTerminal ++ " --title Ranger -e ranger")
  , ("M-d", spawn "rofi -show drun")
  , ("M-S-d", spawn "rofi -show run")
  , ("M-p", spawn "passmenu -p pass")
  , ("M-w", spawn "brave")
  , ("M-S-w", spawn "brave --incognito")
  , ("M-S-f", spawn "pcmanfm")
  , ("M-s", spawn "spotify")
  , ("M-t", spawn "transmission-gtk")
  , ("<Print>", spawn "flameshot gui")
  , ("M-e", spawn "emacsclient -c -a 'emacs'")
  , ("M-C-d", spawn "def-lookup.sh")
  , ("M-z", spawn "xkb-switch -n")
  , ("M-q", kill)

  -- scratchpads
  , ("M-g", namedScratchpadAction myScratchPads "gcolor")
  , ("M-c", namedScratchpadAction myScratchPads "galculator")
  , ("M-y", namedScratchpadAction myScratchPads "bashtop")
  , ("M-r", namedScratchpadAction myScratchPads "calendar")
  , ("M-b", namedScratchpadAction myScratchPads "brightness")
  , ("M-S-c", namedScratchpadAction myScratchPads "caprine")

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

  -- layout controls
  , ("M-a", sendMessage $ Toggle NBFULL)
  , ("M-S-a", sendMessage ToggleStruts)
  , ("M-n", sendMessage NextLayout)
  , ("M-m", spawn "xdotool key super+a && xdotool key super+A")

  -- workspace controls
  , ("M-<Left>", moveTo Prev workspaceOnCurrentScreen)
  , ("M-<Right>", moveTo Next workspaceOnCurrentScreen)

  -- screen controll
  , ("M-o", switchScreen 1)
  , ("M-S-o", shiftNextScreen)

  -- kill / restart xmonad
  , ("M-S-q", io exitSuccess)
  , ("M-S-r", spawn "killall xmobar; xmonad --recompile; xmonad --restart")

  ]


myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
 [((m .|. modm, k), windows $ onCurrentScreen f i)
 | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
 , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
 ]


myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , ((modm .|. shiftMask, button1), dragWindow)
  , ((modm, button2), const kill)
  , ((modm, button3), \w -> focus w >> Flex.mouseResizeWindow w)
  , ((modm, button4), \_ -> moveTo Prev workspaceOnCurrentScreen)
  , ((modm, button5), \_ -> moveTo Next workspaceOnCurrentScreen)
  ]


------------------------------------------------------------------------
--

switchScreen :: Int -> X ()
switchScreen d = do s <- screenBy d
                    mws <- screenWorkspace s
                    warpToScreen s 0.618 0.618
                    case mws of
                         Nothing -> return ()
                         Just ws -> windows (W.view ws)


------------------------------------------------------------------------
--

mySpacing :: Integer -> Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i j = spacingRaw False (Border i i i i) True (Border j j j j) True

myLayoutHook = avoidStruts $ onWorkspaces ["0_9", "1_9"] layoutGrid $ layoutTall ||| layoutTabbed
  where
    layoutTall = mkToggle (NBFULL ?? EOT) . named "tall" $ draggingVisualizer $ smartBorders $ mySpacing 55 15 $ mouseResizableTile { masterFrac = 0.65, draggerType = FixedDragger 0 30}
    layoutGrid = mkToggle (NBFULL ?? EOT) . named "grid" $ draggingVisualizer $ smartBorders $ mySpacing 55 15 $ Grid False
    layoutTabbed = mkToggle (NBFULL ?? EOT) . named "full" $ smartBorders $ mySpacing 65 5 $ tabbed shrinkText myTabTheme
    myTabTheme = def
      { fontName            = "xft:Roboto:size=12:bold"
      , activeColor         = grey1
      , inactiveColor       = grey1
      , activeBorderColor   = grey1
      , inactiveBorderColor = grey1
      , activeTextColor     = cyan
      , inactiveTextColor   = grey3
      , decoHeight          = 25
      }


------------------------------------------------------------------------
--

(~?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~? x = fmap (x `L.isInfixOf`) q

(/=?) :: Eq a => Query a -> a -> Query Bool
q /=? x = fmap (/= x) q

myManageHook :: ManageHook
myManageHook = composeAll
  [ resource  =? "desktop_window" --> doIgnore
  , isFloat --> doCenterFloat
  , isDialog --> doCenterFloat
  , title =? "Godot Engine" --> doFloat
  , className ~? "(DEBUG)" --> doFloat
  , appName =? "blueman-manager" --> doFloat
  , className =? "awakened-poe-trade" --> doFloat
  , className =? "poe-overlay" --> doFloat
  , className =? "steam_app_238960" --> doFloat
  , appName =? "pavucontrol" -->doFloat
  , title =? myTerminalClass --> insertPosition End Newer
  , insertPosition Master Newer
  ] <+> manageDocks <+> namedScratchpadManageHook myScratchPads


------------------------------------------------------------------------
--

myHandleEventHook :: Event -> X All
myHandleEventHook = multiScreenFocusHook
                 <+> swallowEventHook (className =? myTerminalClass <&&> className =? "Emacs") (return True)
                --  <+> dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> doShift "1_8")


------------------------------------------------------------------------
--

myStartupHook :: X ()
myStartupHook = do
    spawn "killall trayer; trayer --monitor 2 --edge top --align right --widthtype request --padding 7 --iconspacing 10 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x2B2E37  --height 29 --distance 5 &"
    modify $ \xstate -> xstate { windowset = onlyOnScreen 1 "1_1" (windowset xstate) }


------------------------------------------------------------------------
--

newtype MyUpdatePointerActive = MyUpdatePointerActive Bool
instance ExtensionClass MyUpdatePointerActive where
  initialValue = MyUpdatePointerActive True

myUpdatePointer :: (Rational, Rational) -> (Rational, Rational) -> X ()
myUpdatePointer refPos ratio =
  whenX isActive $ do
    dpy <- asks display
    root <- asks theRoot
    (_,_,_,_,_,_,_,m) <- io $ queryPointer dpy root
    unless (testBit m 9 || testBit m 8 || testBit m 10) $ -- unless the mouse is clicking
      updatePointer refPos ratio

  where
    isActive = (\(MyUpdatePointerActive b) -> b) <$> XS.get


------------------------------------------------------------------------
--

multiScreenFocusHook :: Event -> X All
multiScreenFocusHook MotionEvent { ev_x = x, ev_y = y } = do
  ms <- getScreenForPos x y
  case ms of
    Just cursorScreen -> do
      let cursorScreenID = W.screen cursorScreen
      focussedScreenID <- gets (W.screen . W.current . windowset)
      when (cursorScreenID /= focussedScreenID) (focusWS $ W.tag $ W.workspace cursorScreen)
      return (All True)
    _ -> return (All True)
  where getScreenForPos :: CInt -> CInt
            -> X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
        getScreenForPos x y = do
          ws <- windowset <$> get
          let screens = W.current ws : W.visible ws
              inRects = map (inRect x y . screenRect . W.screenDetail) screens
          return $ fst <$> find snd (zip screens inRects)
        inRect :: CInt -> CInt -> Rectangle -> Bool
        inRect x y rect = let l = fromIntegral (rect_x rect)
                              r = l + fromIntegral (rect_width rect)
                              t = fromIntegral (rect_y rect)
                              b = t + fromIntegral (rect_height rect)
                           in x >= l && x < r && y >= t && y < b
        focusWS :: WorkspaceId -> X ()
        focusWS ids = windows (W.view ids)
multiScreenFocusHook _ = return (All True)


------------------------------------------------------------------------
--

myWorkspaceIndices :: M.Map [Char] Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..]

clickable :: [Char] -> [Char] -> [Char]
clickable icon ws = addActions [ (show i, 1), ("q", 2), ("Left", 4), ("Right", 5) ] icon
                    where i = fromJust $ M.lookup ws myWorkspaceIndices

myStatusBarSpawner :: Applicative f => ScreenId -> f StatusBarConfig
myStatusBarSpawner (S s) = do
                    pure $ statusBarPropTo ("_XMONAD_LOG_" ++ show s)
                          ("xmobar -x " ++ show s ++ " ~/.config/xmonad/xmobar/xmobar" ++ show s ++ ".hs")
                          (pure $ myXmobarPP (S s))


myXmobarPP :: ScreenId -> PP
myXmobarPP s  = filterOutWsPP [scratchpadWorkspaceTag] . marshallPP s $ def
  { ppSep = ""
  , ppWsSep = ""
  , ppCurrent = xmobarColor cyan "" . clickable wsIconFull
  , ppVisible = xmobarColor grey4 "" . clickable wsIconFull
  , ppVisibleNoWindows = Just (xmobarColor grey4 "" . clickable wsIconFull)
  , ppHidden = xmobarColor grey2 "" . clickable wsIconHidden
  , ppHiddenNoWindows = xmobarColor grey2 "" . clickable wsIconEmpty
  , ppUrgent = xmobarColor orange "" . clickable wsIconFull
  , ppOrder = \(ws : _ : _ : extras) -> ws : extras
  , ppExtras  = [ wrapL (actionPrefix ++ "n" ++ actionButton ++ "1>") actionSuffix
                $ wrapL (actionPrefix ++ "q" ++ actionButton ++ "2>") actionSuffix
                $ wrapL (actionPrefix ++ "Left" ++ actionButton ++ "4>") actionSuffix
                $ wrapL (actionPrefix ++ "Right" ++ actionButton ++ "5>") actionSuffix
                $ wrapL "    " "    " $ layoutColorIsActive s (logLayoutOnScreen s)
                , wrapL (actionPrefix ++ "q" ++ actionButton ++ "2>") actionSuffix
                $  titleColorIsActive s (shortenL 81 $ logTitleOnScreen s)
                ]
  }
  where
    wsIconFull   = "  <fn=2>\xf111</fn>   "
    wsIconHidden = "  <fn=2>\xf111</fn>   "
    wsIconEmpty  = "  <fn=2>\xf10c</fn>   "
    titleColorIsActive n l = do
      c <- withWindowSet $ return . W.screen . W.current
      if n == c then xmobarColorL cyan "" l else xmobarColorL grey3 "" l
    layoutColorIsActive n l = do
      c <- withWindowSet $ return . W.screen . W.current
      if n == c then wrapL "<icon=/home/amnesia/.config/xmonad/xmobar/icons/" "_selected.xpm/>" l else wrapL "<icon=/home/amnesia/.config/xmonad/xmobar/icons/" ".xpm/>" l


------------------------------------------------------------------------
--

main :: IO ()
main = xmonad
       . ewmh
       . ewmhFullscreen
       . dynamicSBs myStatusBarSpawner
       . docks
       $ def
        { focusFollowsMouse  = True
        , clickJustFocuses   = False
        , borderWidth        = 3
        , modMask            = mod4Mask
        , normalBorderColor  = grey2
        , focusedBorderColor = cyan
        , terminal           = myTerminal
        , keys               = myKeys
        , workspaces         = withScreens 2 myWorkspaces
        , mouseBindings      = myMouseBindings
        , layoutHook         = myLayoutHook
        , manageHook         = myManageHook
        , startupHook        = myStartupHook

        , rootMask = rootMask def .|. pointerMotionMask
        -- , logHook            = logHook def <+> myUpdatePointer (0.75, 0.75) (0, 0)
        , handleEventHook    = myHandleEventHook
        } `additionalKeysP` myAditionalKeys
