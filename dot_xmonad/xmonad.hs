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
import qualified XMonad.Util.Hacks as Hacks
import Data.Monoid
import System.IO

myModMask = mod1Mask  -- rebind Mod to Super key
myTerminal = "alacritty"
myBorderWidth = 2
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "+"]

myLayoutHook = onWorkspace "1" webLayout $ onWorkspace "4" fullLayout $ layouts
    where layouts = smartBorders $ avoidStruts $ (layoutHook def ||| Grid ||| ThreeCol 1 (5/100) (1/3))
          webLayout = smartBorders $ avoidStruts (Tall 1 (3/100) (70/100) ||| Full)
          fullLayout = smartBorders $ noBorders Full

myManageHook = (composeAll . concat $
    [[isFullscreen              --> doFullFloat
    , className =? "Xmessage"   --> doCenterFloat
    , className =? "Xfce4-notifyd" --> doIgnore -- Fixes notification bubbles grabbing focus
    ]]) <+> manageDocks <+> manageHook def

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
    [ ((myModMask .|. shiftMask, xK_l), spawn "xsecurelock-lock")
    , ((myModMask .|. shiftMask, xK_f), fullFloatFocused)
    , ((myModMask, xK_p), spawn "dmenu_run -i -fn \'-*-fixed-*-*-*-20-*-*-*-*-*-iso8859-15\'")
    , ((0, 0x1008FF11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5% && notify-send Volume 'Lower volume'") -- XF86XK_AudioLowerVolume
    , ((0, 0x1008FF13), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5% && notify-send Volume 'Raise volume'") -- XF86XK_AudioRaiseVolume
    , ((0, 0x1008FF12), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle && amixer set PCM unmute && notify-send Volume 'Toggle mute'") -- XF86XK_AudioMute
    ]
    ++
    [((m .|. myModMask, k), windows $ f i)
    | (i, k) <- zip myWorkspaces [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_plus]
    , (f, m) <- [(greedyView, 0), (shift, shiftMask)]]

fullFloatFocused =
    withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar"
  xmonad $ docks $ ewmhFullscreen $ ewmh def {
               terminal = myTerminal
             , manageHook = myManageHook
             , layoutHook = myLayoutHook
             , borderWidth = myBorderWidth
             , focusedBorderColor = myFocusedBorderColor
             , modMask = myModMask
             , logHook = myLogHook xmproc >> setWMName "LG3D"
             , workspaces = myWorkspaces
             , handleEventHook = Hacks.trayerAboveXmobarEventHook <> Hacks.trayerPaddingXmobarEventHook
             } `additionalKeys` myKeys
