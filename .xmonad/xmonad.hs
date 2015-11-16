import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ThreeColumns
import XMonad.StackSet hiding (workspaces)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import Data.Monoid
import System.IO

myModMask = mod1Mask  -- rebind Mod to Super key
myTerminal = "urxvt"
myBorderWidth = 2
myWorkspaces = ["1:web", "2:irc", "3:terms", "4:term", "5:code", "6:dev",
                "7:misc", "8:music", "9:vbox", "0:empty", "+:empty"]

myLayoutHook = onWorkspace "1:web" webLayout $ onWorkspace "2:irc" noTitleLayout $
               onWorkspace "3:terms" noTitleLayout $ onWorkspace "4:term" fullLayout $
               onWorkspace "9:vbox" fullLayout $ layouts
    where layouts = smartBorders $ avoidStruts $ (layoutHook defaultConfig ||| Grid ||| ThreeCol 1 (3/100) (1/2))
          webLayout = smartBorders $ avoidStruts (Tall 1 (3/100) (70/100) ||| Full)
          fullLayout = smartBorders $ noBorders Full
          noTitleLayout = smartBorders $ avoidStruts (Tall 1 (3/100) (1/2))

myManageHook = (composeAll . concat $
    [[isFullscreen              --> doFullFloat
    , className =? "Xmessage"   --> doCenterFloat
    , className =? "XCalc"      --> doCenterFloat
    , className =? "Zenity"     --> doCenterFloat
    , className =? "feh"        --> doCenterFloat
    , className =? "VirtualBox" --> doShift "9:vbox"
    , className =? "Xfce4-notifyd" --> doIgnore -- Fixes notification bubbles grabbing focus
    , title     =? "Save As..." --> doCenterFloat
    , title     =? "Save File"  --> doCenterFloat
    , title     =? "term_2"  --> doShift "2:irc"
    , title     =? "term_3"  --> doShift "3:terms"
    , title     =? "term_4"  --> doShift "4:term"
    , title     =? "term_5"  --> doShift "5:code"
    , title     =? "term_8"  --> doShift "8:music"
    ]]) <+> manageDocks <+> manageHook defaultConfig

myLogHook xmproc = dynamicLogWithPP $ xmobarPP {
                     ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor lightTextColor ""
                   , ppCurrent = xmobarColor focusColor ""
                   , ppVisible = xmobarColor lightTextColor ""
                   , ppHiddenNoWindows = xmobarColor lightBackgroundColor ""
                   , ppUrgent = xmobarColor myUrgentColor ""
                   , ppSep = " :: "
                   , ppWsSep = " "
                   }

focusColor = "#7878E3"
textColor = "#0A34BF"
lightTextColor = "#8686D1"
backgroundColor = "#5071DE"
lightBackgroundColor = "#241D40"
myUrgentColor = "#ffc000"
myFocusedBorderColor = "#FF0000"

myKeys =
    [ ((myModMask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
    , ((myModMask .|. shiftMask, xK_s), sendMessage ToggleStruts)
    , ((myModMask .|. shiftMask, xK_p), startupPrograms)
    , ((myModMask .|. shiftMask, xK_f), fullFloatFocused)
    , ((myModMask, xK_p), spawn "dmenu_run -i -fn \'-*-fixed-*-*-*-20-*-*-*-*-*-iso8859-15\'")
    , ((myModMask .|. shiftMask, xK_b), spawn "ncmpcpp-status")
    , ((myModMask .|. shiftMask, xK_n), spawn "ncmpcpp pause")
    , ((myModMask .|. shiftMask, xK_m), spawn "ncmpcpp play")
    , ((myModMask .|. shiftMask, xK_comma), spawn "ncmpcpp prev")
    , ((myModMask .|. shiftMask, xK_period), spawn "ncmpcpp next")
    , ((0, 0x1008FF11), spawn "amixer set Master 2- && notify-send Volume 'Lower volume'") -- XF86XK_AudioLowerVolume
    , ((0, 0x1008FF13), spawn "amixer set Master 2+ && notify-send Volume 'Raise volume'") -- XF86XK_AudioRaiseVolume
    , ((0, 0x1008FF12), spawn "amixer set Master toggle && amixer set PCM unmute && notify-send Volume 'Toggle mute'") -- XF86XK_AudioMute
    ]
    -- More workspaces
    ++
    [((m .|. myModMask, k), windows $ f i)
    | (i, k) <- zip myWorkspaces [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_plus]
    , (f, m) <- [(greedyView, 0), (shift, shiftMask)]]

fullFloatFocused =
    withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f

startupPrograms = do
                  spawn (myTerminal ++ " -title term_2")
                  spawn (myTerminal ++ " -title term_3")
                  spawn (myTerminal ++ " -title term_3")
                  spawn (myTerminal ++ " -title term_4")
                  spawn (myTerminal ++ " -title term_5")
                  spawn (myTerminal ++ " -title term_8")
                  spawn (myTerminal ++ " -title term_8")
                  spawn "pidgin"
                  spawnOn "1:web" "firefox"

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar"
  xmonad $ ewmh defaultConfig {
               terminal = myTerminal
             , manageHook = myManageHook
             , layoutHook = myLayoutHook
             , borderWidth = myBorderWidth
             , focusedBorderColor = myFocusedBorderColor
             , modMask = myModMask
             , logHook = myLogHook xmproc >> ewmhDesktopsLogHook >> setWMName "LG3D"
             , workspaces = myWorkspaces
             , handleEventHook = fullscreenEventHook -- Fixes fullscreen in Chromium browser
             } `additionalKeys` myKeys
