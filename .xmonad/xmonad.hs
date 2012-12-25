import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Util.Run (spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.Shell

import Data.Ratio ((%))
import qualified Data.Map as M

import IM

main = do
    xmonad =<< statusBar "xmobar" myPP toggleStrutsKey desktopConfig
        { borderWidth        = 1
        , normalBorderColor  = "#113354"
        , focusedBorderColor = "#6587a8"
        , modMask            = mod4Mask
        , keys               = myKeys <+> keys desktopConfig
        , manageHook         = myManageHook
        , layoutHook         = smartBorders $ avoidStruts $ myLayoutHook
        , startupHook        = myStartupHook
        , logHook            = takeTopFocus } 

myKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_p), shellPrompt myXPConfig)
    -- XF86AudioLowerVolume
    , ((0, 0x1008ff11), spawn "amixer -q set Master 1-")
    -- XF86AudioRaiseVolume
    , ((0, 0x1008ff13), spawn "amixer -q set Master 1+ unmute")
    -- XF86AudioPlay
    , ((0, 0x1008ff14), spawn "mocp -U")
    -- XF86AudioStop
    , ((0, 0x1008ff15), spawn "mocp -P")
    -- XF86AudioPrev
    , ((0, 0x1008ff16), spawn "mocp -r")
    -- XF86AudioNext
    , ((0, 0x1008ff17), spawn "mocp -f") ]

myManageHook = composeAll
    [ isFullscreen                  --> doFullFloat
    , className =? "Konsole"        --> doShift "1"
    , className =? "Firefox"        --> doShift "2"
    , className =? "Skype"          --> doShift "3"
    , className =? "psi"            --> doShift "3"
    , className =? "Plasma-desktop" --> doFloat
    , className =? "Plasma"         --> doIgnore
    , className =? "Klipper"        --> doFloat
    , className =? "stalonetray"    --> doIgnore
    , className =? "Wicd-kde" --> doFloat ]

myLayoutHook = onWorkspace "3" (gridIM (1%6) rosterProperty) $ layoutHook desktopConfig

rosterProperty = [psiRoster, skypeRoster]

psiRoster = (And (ClassName "psi") (Resource "main"))

skypeRoster = (And (ClassName "Skype") (Title "mityada - Skype™"))

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
    { font = "-xos4-terminus-medium-r-normal-*-14-*-*-*-*-*-*-*"
    , bgColor = "black"
    , fgColor = "#afafaf"
    , fgHLight = "white"
    , bgHLight = "black"
    , borderColor = "#113354"
    , height = 21
    }

toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
