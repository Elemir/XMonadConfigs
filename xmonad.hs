{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}
--
-- xmonad Elemir's config file.
--

import XMonad
import XMonad.Actions.FocusNth
import XMonad.Hooks.AutoCentering
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Gaps
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.LayoutCombinators
import XMonad.Util.Cursor(setDefaultCursor)
import XMonad.Util.XUtils(fi)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WindowProperties
import XMonad.Util.NamedWindows (getName)
import XMonad.StackSet (floating, member)
import Data.Bits
import Data.List
import Data.Monoid
import Data.Ratio ((%))
import System.Exit
import System.IO

import Control.Monad.Reader

import XMonad.Layout.Tabbed
import XMonad.Layout.MTabbed
import XMonad.Layout.VTabbed

import qualified XMonad.StackSet      as W
import qualified Data.Map             as M

instance Namer CustomNamer where
    nameIt _ w = do
                    ws <- gets windowset
                    nw <- getName w
                    let num = maybe "" (\x -> (show $ x + 1) ++ ": ") $ elemIndex w (W.integrate' $ W.stack
                                                                          $ W.workspace $ W.current ws)
                    return $ num ++ (show nw)

myName = CustomNamer

myFont = "-xos4-terminus-bold-r-normal--16-160-72-72-c-80-iso10646-1"
myXftFont = "Terminus:size=12:bold"

myFocusTextColor        = "#ff8800" -- "#ffffff"
myFocusBGColor          = "#000000" -- "#009436"
myFocusBorderColor      = "#d3d3d3" -- "#ffffff"
myNormalTextColor       = "#d3d3d3" -- "#ffffff"
myNormalBGColor         = "#000000" -- "#3d47ff"
myNormalBorderColor     = "#d3d3d3" -- "#ffffff"

myDMenuCommand = "dmenu_run" ++ " -fn " ++ "'" ++ myXftFont ++ "'"
                             ++ " -nb " ++ "'" ++ myNormalBGColor ++ "'"
                             ++ " -nf " ++ "'" ++ myNormalTextColor ++ "'"
                             ++ " -sb " ++ "'" ++ myFocusBGColor ++ "'"
                             ++ " -sf " ++ "'" ++ myFocusTextColor ++ "'"

myTheme :: Theme
myTheme = defaultTheme { activeBorderColor   = myFocusBorderColor
                       , inactiveBorderColor = myNormalBorderColor
                       , activeColor         = myFocusBGColor
                       , inactiveColor       = myNormalBGColor
                       , activeTextColor     = myFocusTextColor
                       , inactiveTextColor   = myNormalTextColor
                       , fontName            = "xft:" ++ myXftFont
                       , decoHeight          = 19
}

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn myDMenuCommand)
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm .|. shiftMask, xK_m     ), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ((modm              , xK_q     ), spawn "xmonad --restart")
    
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((modm, k), focusNth i)
        | (i, k) <- zip [0..11] [xK_F1 .. xK_F12]]


myMouseBindings _ = M.fromList $ []

myLayout = avoidStruts $ onWorkspace "1: Terminals" termLayout $ onWorkspace "2: Browser" mtabLayout $ standardLayout
    where
        standardLayout = Full
        termLayout = stackLayout  -- *||* stackLayout
        --vtabLayout = vtabbed 200 shrinkText myTheme
        mtabLayout = mtabbed 8 myName shrinkText myTheme
        stackLayout = mtabbed 1 myName shrinkText myTheme
--        imLayout = withIM (3 % 12) tkabberRoster mtabLayout
--        skypeLayout = withIM (2 % 11) skypeRoster mtabLayout
--        tkabberRoster = ClassName "Tkabber"
--        skypeRoster = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm") `And` (Not (Title "File Transfers")))


myManageHook = composeAll 
    [ className =? "Firefox"        --> doShift "2: Browser"
    , className =? "Apvlv"          --> doShift "5: Multimedia"
    , className =? "Apvlv"          --> doShift "4: GVim"
    , className =? "mplayer2"       --> viewShift "5: Multimedia" 
    , isFullscreen                  --> doFullFloat
    , isDialog                      --> doCenterFloat
    , className =? "Pqiv"           --> viewShift "5: Multimedia" <+> doFullFloat
    , className =? "Wine"           --> doShift "6: Emulators" <+> doCenterFloat
    , className =? "qemu"           --> doShift "6: Emulators" <+> doCenterFloat
    , className =? "XDosEmu"        --> doShift "6: Emulators" <+> doCenterFloat
    , className =? "Xmessage"       --> doCenterFloat
    , appName   =? "URxvtDesktop"   --> doIgnore
    ]
  where viewShift = doF . liftM2 (.) W.greedyView W.shift

main = do
    h <- spawnPipe $ "xmobar ~/.xmonad/xmobarrc"
    xmonad $ ewmh $ defaultConfig
      { terminal           = "urxvtc"

      , focusFollowsMouse  = False
      , borderWidth        = 1
      , modMask            = mod4Mask
      , workspaces         = [ "1: Terminals", "2: Browser", "3: Desktop", "4: GVim", "5: Multimedia", "6: Emulators", "7: Other" ]
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusBorderColor

      , keys               = myKeys
      , mouseBindings      = myMouseBindings
      , layoutHook         = gaps [(R, 0)] $ myLayout
      , logHook            = dynamicLogWithPP $ xmobarPP { ppOutput  = hPutStrLn h 
                                                       , ppCurrent = xmobarColor myFocusTextColor myFocusBGColor . pad
                                                       , ppHidden  = xmobarColor myNormalTextColor myNormalBGColor . pad
                                                       , ppWsSep   = "│"
                                                       , ppSep     = "│"
                                                       , ppTitle   = const " "
                                                       , ppLayout  = const ""
                                                       }

      , manageHook         = myManageHook <+> manageDocks
      , handleEventHook    = docksEventHook
      , startupHook        = setDefaultCursor xC_left_ptr <+> setWMName "LG3D"
   }

