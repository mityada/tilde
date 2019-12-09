import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Util.Run (spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.SpawnOn

import Data.Ratio ((%))
import qualified Data.Map as M

import IM

main = do
    xmonad =<< statusBar "xmobar" myPP toggleStrutsKey (withUrgencyHook NoUrgencyHook desktopConfig
        { borderWidth        = 1
        , normalBorderColor  = "#113354"
        , focusedBorderColor = "#6587a8"
        , modMask            = mod4Mask
        , keys               = myKeys <+> keys desktopConfig
        , manageHook         = myManageHook
        , layoutHook         = smartBorders $ avoidStruts $ myLayoutHook
        , startupHook        = myStartupHook
        , logHook            = takeTopFocus
        , handleEventHook    = docksEventHook <+> fullscreenEventHook } )

myKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_p), shellPromptHere myXPConfig)
    , ((modm, xK_x), spawn "light-locker-command -l")
    , ((0, xK_Print), spawn "import -window root ~/Pictures/screenshots/$(date '+%Y%m%d-%H%M%S').png")
    -- XF86AudioLowerVolume
    , ((0, 0x1008ff11), spawn "amixer -q set Master 1%-")
    -- XF86AudioRaiseVolume
    , ((0, 0x1008ff13), spawn "amixer -q set Master 1%+ unmute")
    -- XF86AudioMute
    , ((0, 0x1008ff12), spawn "amixer -q set Master toggle")
    -- XF86AudioPlay
    , ((0, 0x1008ff14), spawn "mocp-control play")
    -- XF86AudioStop
    , ((0, 0x1008ff15), spawn "mocp-control stop")
    -- XF86AudioPrev
    , ((0, 0x1008ff16), spawn "mocp -r")
    -- XF86AudioNext
    , ((0, 0x1008ff17), spawn "mocp -f") ]

myManageHook = composeAll
    [ isFullscreen                  --> doFullFloat
    , className =? "xterm"          --> doShift "1"
    , className =? "Firefox"        --> doShift "2"
    , className =? "Skype"          --> doShift "3"
    , className =? "Psi-plus"       --> doShift "3"
    , className =? "qTox"           --> doShift "3"
    , className =? "ViberPC"        --> doShift "3"
    , className =? "Logic"          --> doIgnore ]
    -- , className =? "Gimp"           --> doFloat ]

myLayoutHook = onWorkspace "3" (reflectHoriz $ gridIM (1%6) rosterProperty) $ layoutHook desktopConfig

rosterProperty = [psiRoster, skypeRoster, toxRoster]

psiRoster = (And (ClassName "Psi-plus") (Resource "main"))

skypeRoster = (And (ClassName "Skype") (Title "mityada - Skype™"))

toxRoster = (And (ClassName "qTox") (Title "qTox"))

myStartupHook = do
    setWMName "LG3D"

myPP = xmobarPP
    { ppCurrent = xmobarColor "#6587a8" "" 
    , ppUrgent  = xmobarColor "#ca1f7b" ""
    , ppHidden  = xmobarColor "#afafaf" ""
    , ppTitle   = xmobarColor "#ffffff" "" . shorten 80
    , ppLayout  = xmobarColor "#ffffff" ""
    , ppSep     = xmobarColor "#6587a8" "" " >> " }

myXPConfig = defaultXPConfig
    { font = "xft:xos4 Terminus:style=Regular:pixelsize=24"
    , bgColor = "black"
    , fgColor = "#afafaf"
    , fgHLight = "white"
    , bgHLight = "black"
    , borderColor = "#113354"
    , height = 32
    }

toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
