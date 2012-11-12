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
import XMonad.Layout.IM
import XMonad.Util.Run (spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.Shell

import Data.Ratio ((%))
import qualified Data.Map as M

main = do
    xmonad =<< statusBar "xmobar" myPP toggleStrutsKey desktopConfig
        { borderWidth        = 1
        , normalBorderColor  = "#113354"
        , focusedBorderColor = "#6587a8"
        , modMask            = mod4Mask
        , keys               = myKeys <+> keys desktopConfig
--        , workspaces         = myWorkspaces
        , manageHook         = ((className =? "java-lang-Thread") >>= return . not --> manageHook desktopConfig) <+> myManageHook
        , layoutHook         = smartBorders $ avoidStruts $ myLayoutHook
        , startupHook        = myStartupHook
        , logHook            = takeTopFocus } 

myKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_p)              , shellPrompt myXPConfig)
    , ((modm .|. shiftMask, xK_q), spawn "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:-1 int32:-1 int32:-1") ]

-- myWorkspaces = ["1:term", "2:web", "3:im", "4:other"] ++ map show [5..9]

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

myLayoutHook = onWorkspace "3" (gridIM (1%7) rosterProperty) $ layoutHook desktopConfig

rosterProperty = psiRoster 

psiRoster = (And (ClassName "psi") (Resource "main"))

skypeRoster = (And (ClassName "Skype") (Not (Resource "ConversationsWindow")))

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
