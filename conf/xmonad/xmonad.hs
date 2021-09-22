{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import System.Exit
import System.IO
import XMonad hiding (mouseResizeWindow)
import XMonad.Actions.ConstrainedResize
import XMonad.Actions.GroupNavigation
import XMonad.Actions.Minimize
import XMonad.Actions.Navigation2D
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.ButtonDecoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Drawer
import XMonad.Layout.FixedColumn
import qualified XMonad.Layout.GridVariants as G
import XMonad.Layout.HintedGrid
import XMonad.Layout.MagicFocus
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Dzen as D
import XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.Themes

main = do
  spawn "urxvtd -q -o -f"
  -- spawn "autorandr --load default"
  xmproc <- spawnPipe "xmobar -x 0"

  let barLogHook =
        myLogHook
          >> dynamicLogWithPP myPP{ppOutput = hPutStrLn xmproc}

  let barC = docks $ myConfig{logHook = barLogHook}
  let nav2dC = withNavigation2DConfig myNav2DConfig barC
  let ewmhC = ewmh nav2dC

  xmonad ewmhC

-------------------------------------------------
--------------------- Theme ---------------------
-------------------------------------------------

myFontSize :: (Integral i, Show i) => i -> String
myFontSize size = "xft:Tamsyn:size=" ++ show size

myFont = myFontSize 12

primaryColor = "#0f8cf2"
secondaryColor = "#a5ab00"

myThemeSize size =
  def
    { activeColor = "black"
    , inactiveColor = "black"
    , urgentColor = "black"
    , urgentBorderColor = "red"
    , activeTextColor = secondaryColor
    , inactiveTextColor = primaryColor
    , urgentTextColor = "red"
    , activeBorderWidth = 0
    , inactiveBorderWidth = 0
    , urgentBorderWidth = 1
    , fontName = myFontSize size
    }

myConfig =
  def
    { terminal = "urxvtc"
    , modMask = mod4Mask
    , borderWidth = 2
    , keys = myKeys
    , workspaces = myWorkspaces
    , mouseBindings = myMouseBindings
    , normalBorderColor = primaryColor
    , focusedBorderColor = secondaryColor
    , startupHook = myStartupHook
    , layoutHook = myLayouts
    , manageHook = myManageHook <+> manageHook def
    -- , handleEventHook = myHandleEventHook
    }

myXPConfig =
  def
    { font = myFont
    , bgColor = "black"
    , bgHLight = "black"
    , fgColor = primaryColor
    , fgHLight = secondaryColor
    , alwaysHighlight = True
    , historyFilter = uniqSort
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    }

myNav2DConfig =
  def
    { defaultTiledNavigation = centerNavigation
    }

myKeys conf =
  mkKeymap conf $
    [ ("M-S-<Backspace>", confirmPrompt myXPConfig "exit" $ io $ exitWith ExitSuccess)
    , ("M-<Backspace>", releaseRestart)
    , ("M-S-q", kill)
    , ("M-S-<Return>", spawn $ terminal conf)
    , ("M-p", spawn "emacsclient -c -n")
    , ("M-h", sendMessage Shrink)
    , ("M-j", BW.focusDown)
    , ("M-k", BW.focusUp)
    , ("M-l", sendMessage Expand)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-m", BW.focusMaster)
    , ("M-<Return>", windows W.swapMaster)
    , ("M-,", sendMessage $ IncMasterN 1)
    , ("M-.", sendMessage $ IncMasterN (-1))
    , ("M-<Esc>", sendMessage FirstLayout)
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-S-<Space>", setLayout $ layoutHook conf)
    , ("M-t", withFocused $ windows . W.sink)
    , ("M-S-t", withFocused float)
    , ("M-=", BW.markBoring)
    , ("M-S-=", BW.clearBoring)
    , ("M--", withFocused minimizeWindow)
    , ("M-S--", withLastMinimized maximizeWindowAndFocus)
    , ("M-f", withFocused (sendMessage . maximizeRestore))
    , ("M-x", banish LowerLeft)
    , ("M-S-x", warpToWindow 0.5 0.5)
    , ("M-o", sendMessage Mag.Toggle)
    , ("M-[", sendMessage Mag.MagnifyLess)
    , ("M-]", sendMessage Mag.MagnifyMore)
    , ("M-b", sendMessage ToggleStruts)
    , ("M-S-f", sendMessage ToggleStruts >> (sendMessage $ Toggle NBFULL))
    , ("M-c", sendMessage $ Toggle MIRROR)
    , ("M-v", sendMessage $ Toggle REFLECTX)
    , ("M-d", shellPrompt myXPConfig)
    , ("M-S-d", sshPrompt myXPConfig)
    , ("M-a", windowPrompt myXPConfig Goto allWindows)
    , ("M-S-a", windowPrompt myXPConfig Bring allWindows)
    , ("M-g", workspacePrompt myXPConfig (windows . W.greedyView))
    , ("M-S-g", workspacePrompt myXPConfig (windows . W.shift))
    , ("M-<F1>", spawn "flameshot gui -p ~/ss")
    , ("M-<F2>", spawn "xautolock -locknow")
    , ("M-<F3>", spawn "kill -s USR1 $(pidof dunst)") -- Disable notifs
    , ("M-<F4>", spawn "kill -s USR2 $(pidof dunst)") -- Enable notifs
    , ("M-<F5>", spawn "systemctl suspend")
    , ("M-<Tab>", nextMatch History $ return True)
    , ("M-C-h", windowGo L False)
    , ("M-C-j", windowGo D False)
    , ("M-C-k", windowGo U False)
    , ("M-C-l", windowGo R False)
    , ("M-M1-h", sendMessage $ ExpandTowards L)
    , ("M-M1-j", sendMessage $ ExpandTowards D)
    , ("M-M1-k", sendMessage $ ExpandTowards U)
    , ("M-M1-l", sendMessage $ ExpandTowards R)
    , ("M-M1-C-h", sendMessage $ ShrinkFrom R)
    , ("M-M1-C-j", sendMessage $ ShrinkFrom U)
    , ("M-M1-C-k", sendMessage $ ShrinkFrom D)
    , ("M-M1-C-l", sendMessage $ ShrinkFrom L)
    , ("M-S-C-j", sendMessage $ SplitShift Next)
    , ("M-S-C-k", sendMessage $ SplitShift Prev)
    , ("M-s", sendMessage Swap)
    , ("M-S-s", sendMessage Rotate)
    , ("M-`", sendMessage Balance)
    , ("M-S-`", sendMessage Equalize)
    , ("M-n", sendMessage FocusParent)
    , ("M-C-n", sendMessage SelectNode)
    , ("M-S-n", sendMessage MoveNode)
    , ("<XF86AudioMute>", spawn "pamixer -t")
    , ("<XF86AudioMicMute>", spawn "pamixer --default-source -t")
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioStop>", spawn "playerctl stop")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    ]
      ++ myWorkspaceKeys conf
      ++ myMonitorKeys

myWorkspaceKeys conf =
  [ ("M-" ++ mask ++ [key], windows $ action ws)
  | (key, ws) <- zip ['1' .. '9'] (workspaces conf)
  , (action, mask) <- [(W.greedyView, ""), (W.shift, "S-")]
  ]

myMonitorKeys =
  [ ( "M-" ++ mask ++ [key]
    , screenWorkspace scr >>= flip whenJust (windows . action)
    )
  | (key, scr) <- zip "we" [1, 0]
  , (action, mask) <- [(W.view, ""), (W.shift, "S-")]
  ]

myMouseBindings XConfig{modMask = modMask} =
  M.fromList $
    -- Float and move window
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , -- Warp to current window
      ((modMask, button2), (\_ -> warpToWindow 0.5 0.5))
    , -- Float and resize window
      ((modMask, button3), (\w -> focus w >> mouseResizeWindow w False))
    , -- Float and constrained resize window
      ((modMask .|. shiftMask, button3), (\w -> focus w >> mouseResizeWindow w True))
    ]

myLayouts =
  mkToggle (NBFULL ?? FULL ?? REFLECTX ?? NOBORDERS ?? MIRROR ?? EOT)
    . trackFloating
    . minimize
    . maximize
    . BW.boringWindows
    . avoidStruts
    . spacingRaw True (Border 0 0 0 0) False (Border 5 5 5 5) True
    $ Mag.magnifiercz 1.25 emptyBSP
      ||| reflectHoriz (noBorders (tabbed shrinkText $ myThemeSize 9))
      ||| Mag.magnifiercz 1.25 (Tall nmaster delta ratio)
      ||| reflectHoriz (multiCol [1] 1 0.01 (-0.5))
      ||| G.SplitGrid G.R 1 1 (1 / 4) (16 / 10) (5 / 100)
      ||| Accordion
      ||| Grid False
      ||| Full
 where
  drawer = simpleDrawer 0.01 0.3 $ ClassName "discord" `Or` ClassName "Slack"

  nmaster = 1
  delta = 2 / 100
  ratio = 1 / 2

myStartupHook = notify "Startup successful!"

myWorkspaces = ["www", "dev", "misc", "virt", "5", "6", "7", "8", "9"]

myPP =
  xmobarPP
    { ppCurrent = xmobarColor secondaryColor "" . wrap "[" "]"
    , ppHiddenNoWindows = \_ -> ""
    , ppVisibleNoWindows = Nothing
    , ppTitle = xmobarColor "green" "" . shorten 40
    , ppVisible = wrap "(" ")"
    , ppUrgent = xmobarColor "red" "yellow"
    , ppSep = pad "|"
    , ppLayout = myPPLayout
    }

myLogHook = do
  historyHook
  pointerFollowsFocus

myHandleEventHook = focusFollowsPointer

myManageHook =
  composeAll
    [ title =? "Origin" --> doFloat
    , manageDocks
    ]

-- Misc. Functions --
useFocusFollowsPointer = fmap (not . L.isSuffixOf "Accordion") currentLayout

usePointerFollowsFocus = useFocusFollowsPointer

focusFollowsPointer = followOnlyIf useFocusFollowsPointer

pointerFollowsFocus = whenX usePointerFollowsFocus $ updatePointer (0.85, 0.15) (0, 0)

releaseRestart = broadcastMessage ReleaseResources >> restart "xmonad" True

notify msg = spawn $ "notify-send 'XMonad' '" ++ msg ++ "'"

currentLayout = gets $ description . W.layout . W.workspace . W.current . windowset

-- Reduce the layout string to whatever comes after "Spacing"
myPPLayout str =
  let split = words str
      idx = L.elemIndex "Spacing" split
      dropSplit = flip L.drop split . (+) 1
      cleaned = unwords $ maybe [str] dropSplit idx
   in cleaned
