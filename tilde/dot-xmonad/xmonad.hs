import Data.Map ( fromList )
import Data.Monoid ( Endo )
import System.Exit ( exitSuccess )

import XMonad
import XMonad.Config
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Hooks.ManageDocks ( avoidStruts, manageDocks )
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog ( xmobar )
import XMonad.Hooks.RefocusLast 

import XMonad.Hooks.ManageHelpers ( composeOne, (-?>), isDialog, doCenterFloat, transience, doSideFloat, Side( SW, SE ) )

import XMonad.Actions.Launcher
import XMonad.Actions.GroupNavigation ( Direction( History ), historyHook, nextMatch )
import XMonad.Layout.MultiToggle ( mkToggle, single, Toggle(Toggle) )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers( FULL ) )
import XMonad.Layout.BinarySpacePartition ( emptyBSP )
import XMonad.Util.EZConfig ( additionalKeysP, removeKeysP )
import XMonad.Prompt ( font, promptBorderWidth, alwaysHighlight, position, XPPosition( Top ), height, XPConfig, sorter, searchPredicate )
import XMonad.Prompt.ConfirmPrompt ( confirmPrompt )
import XMonad.Prompt.Shell ( shellPrompt )
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleRecentWS ( cycleWindowSets )
import XMonad.Actions.FocusNth ( focusNth' )
import XMonad.Prompt.FuzzyMatch as Fuzzy

superKey = mod1Mask

myFont = "'xos4 Terminus:size=14'"
stConfig = "" --" -f " ++ myFont

termCommand = "st" ++ stConfig

launcherConfig = LauncherConfig
    { 
      pathToHoogle = "/home/jephron/.local/bin/hoogle"
    , browser = "firefox"
    }

myConfig = def
            {
                terminal            = termCommand
              , focusFollowsMouse   = False
              , modMask             = mod1Mask
              , startupHook = setWMName "LG3D"
              , focusedBorderColor  = "#335566"
              , normalBorderColor   = "#181000"
              , logHook             = refocusLastLogHook <+> historyHook
              , layoutHook          = avoidStruts $ smartBorders $ myLayout
              , manageHook          = myManageHook <+> manageHook defaultConfig <+> manageDocks
            } `additionalKeysP` [
                ("M-S-t", spawn "st")                                                     -- new terminal
              , ("M-S-<Return>",trySpawnShellAtWindowCwd)                                 -- new terminal at cwd
              , ("M-C-q", spawn "slock")                                                  -- lock the screen
              , ("M-<Space>",   shellPrompt myXPConfig)                                         -- app launcher
              , ("M-C-f", sendMessage $ Toggle FULL)                                      -- toggle fullscreen
              -- , ("M-m",   nextMatch History (return True))                                -- MRU
              , ("M-S-s", withFocused $ windows . W.sink)                                 -- Push window back into tiling
              , ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess))               -- confirm quit x
              -- applications
              , ("M-x M-f", spawn "firefox")                                              -- Firefox
              , ("M-x M-r", spawn (termCommand ++ " ranger"))                             -- Ranger file browser
              , ("M-x M-i", spawn "idea")                                                 -- IntelliJ IDEA
              , ("M-x M-e", spawn "emacs")                                       -- Emacs
              , ("M-x M-l", launcherPrompt myXPConfig $ defaultLauncherModes launcherConfig)
              , ("M-S-p",   spawn "ffcast -s png ~/Screenshots/\"$(date +%F\\ %T)\".png") -- take screenshot
              , ("<XF86AudioLowerVolume>" , spawn "pactl set-sink-mute 0 false; pactl set-sink-volume 0 -5%")
              , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-mute 0 false; pactl set-sink-volume 0 +5%")
              , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
              --, ("M-S-<Tab>", cycleRecentWS' [xK_Super_L, xK_Shift_L] xK_Tab xK_grave) ppp PPP
            ] `removeKeysP` [
                "M-<Return>",
                "M-S-a",
                "M-S-o",
                "M-S-p",
                "M-S-r",
                "M-r",
                "M-p"
            ]

-- modified variant of cycleRecentWS from XMonad.Actions.CycleRecentWS (17)
-- which does not include visible but non-focused workspaces in the cycle
cycleRecentWS' = foo options
 where options w = map (W.view `flip` w) (recentTags w)
       recentTags w = map W.tag $ W.hidden w ++ [W.workspace (W.current w)]


-- todo: cycleWindowFocus
foo :: (WindowSet -> [WindowSet])
                -> [KeySym]
                -> KeySym
                -> KeySym
                -> X ()
foo genOptions mods keyNext keyPrev = do
  options <- gets $ genOptions . windowset
  XConf {theRoot = root, display = d} <- ask
  let event = allocaXEvent $ \p -> do
                maskEvent d (keyPressMask .|. keyReleaseMask) p
                KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
                s <- keycodeToKeysym d c 0
                return (t, s)
  let setOption n = do windows $ W.modify' $ focusNth' (n `mod` 3)
                       --windowSet <- withWindowSet
                       (t, s) <- io event
                       case () of
                         () | t == keyPress   && s == keyNext  -> setOption (n+1)
                            | t == keyPress   && s == keyPrev  -> setOption (n-1)
                            | t == keyRelease && s `elem` mods -> return ()
                            | otherwise                        -> setOption n
  io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
  setOption 0
  io $ ungrabKeyboard d currentTime


-- try to guess the current working directory
-- from the window's title and then spawn st there
-- otherwise just open a terminal at the default path
trySpawnShellAtWindowCwd :: X()
trySpawnShellAtWindowCwd =
    let
        spawnShellAtWindowCwd win = do
            app <- runQuery appName win
            case app of -- is the current window running an st shell?
                "st-256color" -> do
                    title <- runQuery title win
                    let cwd = unwords $ tail $ words title
                    spawn $ termCommand ++ " fish -C \"cd '" ++ cwd ++ "'\""
                _ -> spawn termCommand 
    in
        withWindowSet $ \w -> case (W.peek w) of
            Just win -> spawnShellAtWindowCwd win
            Nothing -> spawn "st" -- handling the case where no window is active

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves
myXPConfig::XPConfig
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:xos4 Terminus:size=14"
  , height            = 48
  , searchPredicate   = Fuzzy.fuzzyMatch
  , sorter            = Fuzzy.fuzzySort
  }

--------------------------------------------------------------------------------
-- | Customize layouts
myLayout = mkToggle (single FULL) (tiled ||| Mirror tiled ||| emptyBSP)
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled = Tall nmaster delta ratio

        -- default number of windows in the master pane
        nmaster = 1

        -- default proportion of the screen occupied by master pane
        ratio = 1/2

        -- percent of screen to increment by resizing panes
        delta = 3/100


doX :: X () -> Query (Endo WindowSet)
doX x = (liftX x) >> (doF id)

--------------------------------------------------------------------------------
myManageHook = composeAll
  [ className =? "mpv"    --> doFloat
  -- , className =? "processing-core-PApplet"   --> doSideFloat SW
  -- , className =? "processing-core-PApplet"   --> doF (fmap W.sink . W.peek )
  , isDialog              --> doCenterFloat
  , title =? "FOOBAR"     --> doSideFloat SE <> (doF W.focusDown)-- >> doX (toggleFocus)  --  >> >> --doF (W.focusMaster)
  
  -- , transience                                   -- Move transient windows to their parent:
  ]

--------------------------------------------------------------------------------
main = do
    xmonad =<< xmobar myConfig

