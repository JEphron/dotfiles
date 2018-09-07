import Data.Map ( fromList )
import System.Exit ( exitSuccess )

import XMonad
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Hooks.ManageDocks ( avoidStruts, manageDocks )
import XMonad.Hooks.DynamicLog ( xmobar )
import XMonad.Hooks.ManageHelpers ( composeOne, (-?>), isDialog, doCenterFloat, transience )

import XMonad.Actions.GroupNavigation ( Direction( History ), historyHook, nextMatch )
import XMonad.Layout.MultiToggle ( mkToggle, single, Toggle(Toggle) )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers( FULL ) )
import XMonad.Layout.BinarySpacePartition ( emptyBSP )
import XMonad.Util.EZConfig ( additionalKeysP ) 
import XMonad.Prompt ( font, promptBorderWidth, alwaysHighlight, position, XPPosition( Top ) )
import XMonad.Prompt.ConfirmPrompt ( confirmPrompt )
import XMonad.Prompt.Shell ( shellPrompt )
import qualified XMonad.StackSet as W


superKey = mod1Mask

myFont = "'xos4 Terminus:size=8'"
stConfig = " -f " ++ myFont

myConfig = def
            {
                terminal            = "st" ++ stConfig
              , focusFollowsMouse   = False
              , modMask             = superKey
              , focusedBorderColor  = "#335566"
              , normalBorderColor   = "#181000"
              , logHook             = historyHook
              , layoutHook          = avoidStruts $ smartBorders $ myLayout
              , manageHook          = myManageHook <+> manageHook defaultConfig <+> manageDocks
            } `additionalKeysP` [
                ("M-S-t", spawn "st")                                           -- new terminal
              , ("M-t",   trySpawnShellAtWindowCwd)                             -- new terminal at cwd
              , ("M-C-q", spawn "slock")                                        -- lock the screen
              , ("M-p",   shellPrompt myXPConfig)                               -- app launcher
              , ("M-S-f", sendMessage $ Toggle FULL)                            -- toggle fullscreen
              , ("M-m",   nextMatch History (return True))                      -- MRU
              , ("M-S-s", withFocused $ windows . W.sink)                       -- Push window back into tiling
              , ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess))     -- confirm quit x
              -- applications
              , ("M-x M-f", spawn "firefox-nightly")                              -- spawn firefox
              , ("M-x M-r", spawn "st ranger")                                    -- spawn ranger
              , ("M-x M-e", spawn "emacs")                                      -- spawn emacs
              , ("<XF86AudioLowerVolume>",   spawn "amixer sset Master 10-")
              , ("<XF86AudioRaiseVolume>",   spawn "amixer sset Master 10+")
          ]


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
					spawn $ "st fish -C \"cd '" ++ cwd ++ "'\""
				_ -> spawn "st"
    in 
        withWindowSet $ \w -> case (W.peek w) of
            Just win -> spawnShellAtWindowCwd win
            Nothing -> spawn "st" -- handling the case where no window is active

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = myFont
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


--------------------------------------------------------------------------------
myManageHook = composeOne
  [ className =? "mpv"    -?> doFloat
  , isDialog              -?> doCenterFloat
  , transience																 -- Move transient windows to their parent:
  ]

--------------------------------------------------------------------------------
main = do
	xmonad =<< xmobar myConfig

