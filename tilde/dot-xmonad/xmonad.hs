{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad (void)
import Data.Monoid (Endo)
import qualified Data.Text.Lazy as Text
import System.Exit (exitSuccess)
import System.Process (spawnCommand)
import Text.Pretty.Simple as PS
import qualified Ultrawide
import XMonad
import XMonad.Actions.CycleRecentWS (cycleRecentWS)
import XMonad.Actions.FocusNth (focusNth')
import XMonad.Actions.GroupNavigation (Direction (History), historyHook, nextMatch)
import XMonad.Actions.Launcher
import XMonad.Config
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers (Side (..), composeOne, doCenterFloat, doSideFloat, isDialog, transience, (-?>))
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.SetWMName
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Column
import XMonad.Layout.MagicFocus
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle (Toggle (Toggle), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing
import XMonad.Prompt (XPConfig, XPPosition (Top), alwaysHighlight, font, height, position, promptBorderWidth, searchPredicate, sorter)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.FuzzyMatch as Fuzzy
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.StackSet
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)

superKey = mod1Mask

myFont = "'xos4 Terminus:size=14'"

stConfig = "" --" -f " ++ myFont

termCommand = "st" ++ stConfig

launcherConfig =
  LauncherConfig
    { pathToHoogle = "/home/jephron/.local/bin/hoogle",
      browser = "firefox"
    }

myConfig =
  def
    { terminal = termCommand,
      focusFollowsMouse = False,
      modMask = mod1Mask,
      startupHook = setWMName "LG3D",
      focusedBorderColor = "#335566",
      normalBorderColor = "#181000",
      logHook = refocusLastLogHook <+> historyHook,
      layoutHook = avoidStruts $ smartBorders $ myLayout,
      -- handleEventHook = promoteWarp' (0.5, 0.5) (0, 0),
      manageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks
    }
    `additionalKeysP` addedKeys
      `removeKeysP` removedKeys
  where
    removedKeys =
      [ "M-<Return>",
        "M-S-a",
        "M-S-p",
        "M-.",
        "M-,",
        "M-S-o",
        "M-S-p",
        "M-S-r",
        "M-r",
        "M-j",
        "M-n",
        "M-m",
        "M-S-j",
        "M-k",
        "C-b",
        "M-p"
      ]
    addedKeys =
      [ ("M-S-t", spawn "st"), -- new terminal
        ("M-S-<Return>", trySpawnShellAtWindowCwd), -- new terminal at cwd
        ("M-C-q", spawn "slock"), -- lock the screen
        ("M-<Space>", shellPrompt myXPConfig), -- app launcher
        ("C-<Space>", sendMessage NextLayout), -- app launcher
        ("M-C-f", sendMessage $ Toggle FULL), -- toggle fullscreen
        ("M-S-s", withFocused $ windows . W.sink), -- Push window back into tiling
        ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess)), -- confirm quit x
        ("M-`", cycleWindowsMRU),
        -- applications
        ("M-x M-f", spawn "firefox"), -- Firefox
        ("M-x M-r", spawn (termCommand ++ " ranger")), -- Ranger file browser
        ("M-x M-i", spawn "idea"), -- IntelliJ IDEA
        ("M-x M-e", spawn "emacs"), -- Emacs
        ("M-x M-l", launcherPrompt myXPConfig $ defaultLauncherModes launcherConfig),
        ("M-C-4", takeScreenshot),
        ("<XF86AudioLowerVolume>", tweakVolume "-5"),
        ("<XF86AudioRaiseVolume>", tweakVolume "+5"),
        ("<XF86AudioMute>", toggleAudioMute)
      ]

playSound :: String -> String
playSound sound =
  "paplay /usr/share/sounds/freedesktop/stereo/" ++ sound ++ ".oga"

takeScreenshot :: X ()
takeScreenshot =
  spawn $ "ffcast -s png ~/Screenshots/\"$(date +%F\\ %T)\".png && " ++ playSound "camera-shutter"

tweakVolume :: String -> X ()
tweakVolume amount =
  spawn $ "pactl set-sink-mute 0 false; pactl set-sink-volume 0 " ++ amount ++ "% && " ++ playSound "audio-volume-change"

toggleAudioMute :: X ()
toggleAudioMute =
  spawn $ "pactl set-sink-mute 0 toggle " ++ playSound "audio-volume-change"

cycleWindowsMRU :: X ()
cycleWindowsMRU =
  let genOptions :: WindowSet -> WindowSet
      genOptions w =
        w
   in do
        options <- gets $ genOptions . windowset
        showDebug options
        return ()

showDebug :: Show a => a -> X ()
showDebug a =
  let content =
        Text.unpack $ PS.pShowNoColor a
   in -- warning: input not escaped properly, use only for debugging
      io $
        void $
          spawnCommand ("echo \"" ++ content ++ "\" | zenity --text-info --width 700 --height 1200")

-- modified variant of cycleRecentWS from XMonad.Actions.CycleRecentWS (17)
-- which does not include visible but non-focused workspaces in the cycle
cycleRecentWS' = foo options
  where
    options w = map (W.view `flip` w) (recentTags w)
    recentTags w = map W.tag $ W.hidden w ++ [W.workspace (W.current w)]

-- todo: cycleWindowFocus
foo ::
  (WindowSet -> [WindowSet]) ->
  [KeySym] ->
  KeySym ->
  KeySym ->
  X ()
foo genOptions mods keyNext keyPrev = do
  options <- gets $ genOptions . windowset
  XConf {theRoot = root, display = d} <- ask
  let event = allocaXEvent $ \p -> do
        maskEvent d (keyPressMask .|. keyReleaseMask) p
        KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
        s <- keycodeToKeysym d c 0
        return (t, s)
  let setOption n = do
        windows $ W.modify' $ focusNth' (n `mod` 3)
        --windowSet <- withWindowSet
        (t, s) <- io event
        case () of
          ()
            | t == keyPress && s == keyNext -> setOption (n + 1)
            | t == keyPress && s == keyPrev -> setOption (n -1)
            | t == keyRelease && s `elem` mods -> return ()
            | otherwise -> setOption n
  io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
  setOption 0
  io $ ungrabKeyboard d currentTime

-- try to guess the current working directory
-- from the window's title and then spawn st there
-- otherwise just open a terminal at the default path
trySpawnShellAtWindowCwd :: X ()
trySpawnShellAtWindowCwd =
  let spawnShellAtWindowCwd win = do
        app <- runQuery appName win
        case app of -- is the current window running an st shell?
          "st-256color" -> do
            title <- runQuery title win
            let cwd = unwords $ tail $ words title
            spawn $ termCommand ++ " fish -C \"cd '" ++ cwd ++ "'\""
          _ -> spawn termCommand
   in withWindowSet $ \w -> case (W.peek w) of
        Just win -> spawnShellAtWindowCwd win
        Nothing -> spawn "st" -- handling the case where no window is active

--------------------------------------------------------------------------------

-- | Customize the way 'XMonad.Prompt' looks and behaves
myXPConfig :: XPConfig
myXPConfig =
  def
    { position = Top,
      alwaysHighlight = True,
      promptBorderWidth = 0,
      font = "xft:xos4 Terminus:size=14",
      height = 48,
      searchPredicate = Fuzzy.fuzzyMatch,
      sorter = Fuzzy.fuzzySort
    }

--------------------------------------------------------------------------------

-- | Customize layouts

-- myLayout = mkToggle (single FULL) (smartSpacing 20 tiled ||| Mirror tiled ||| emptyBSP)
-- myLayout = mkToggle (single FULL) (smartSpacing 5 (Mirror Accordion) ||| smartSpacing 5 tiled ||| Mirror (Column 1))

-- magicFocus: focus follows active window
myLayout = magicFocus $ mkToggle (single FULL) (Ultrawide.UltrawideLayout 1 (3 / 100) (1 / 2))
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio

    -- default number of windows in the master pane
    nmaster = 2

    -- default proportion of the screen occupied by master pane
    ratio = 1 / 2

    -- percent of screen to increment by resizing panes
    delta = 3 / 100

doX :: X () -> Query (Endo WindowSet)
doX x = (liftX x) >> (doF id)

--------------------------------------------------------------------------------
myManageHook =
  composeAll
    [ className =? "mpv" --> doFloat,
      -- , className =? "processing-core-PApplet"   --> doSideFloat SW
      -- , className =? "processing-core-PApplet"   --> doF (fmap W.sink . W.peek )
      isDialog --> doCenterFloat,
      title =? "Hello Haskell" --> doCenterFloat <> doX toggleFocus,
      title =? "FooBAR" --> doSideFloat NE <> (doF W.focusDown), -- >> doX (toggleFocus)  --  >> >> --doF (W.focusMaster)
      className =? "Klobber" --> doCenterFloat,
      className =? "rx" --> doCenterFloat
      -- , transience                                   -- Move transient windows to their parent:
    ]

--------------------------------------------------------------------------------
main = do
  xmonad =<< xmobar myConfig
