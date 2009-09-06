-------------------------------------------------------------------------------
--
-- XMonad Settings (xmonad.hs)
--
-- XMonad Version: 0.8.1
-- Icon Pack: sm4tik (http://koti.24.fi/sm4tik/shared/xbm8x8-0.1.tar.gz)
-- Last Updated: 01.09.2009
--
-- Changes: Code cleanup
--


-- Imports --------------------------------------------------------------------

-- Core
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib
import IO (Handle, hPutStrLn)
import XMonad.Actions.CycleWS (nextScreen,prevScreen)

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Layout

-- Actions
import XMonad.Actions.MouseGestures
import XMonad.Actions.UpdatePointer
import XMonad.Actions.GridSelect

-- Utils
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Loggers

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.DragPane
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.DecorationMadness
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.DwmStyle
import Data.Ratio ((%))
import XMonad.Layout.ToggleLayouts


-- Main -----------------------------------------------------------------------

main = do
       h <- spawnPipe myStatusBar
       xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
              { workspaces = workspaces'
              , modMask = modMask'
              , borderWidth = borderWidth'
              , normalBorderColor = normalBorderColor'
              , focusedBorderColor = focusedBorderColor'
              , terminal = terminal'
              , keys = keys'
              , logHook = logHook' h
              , layoutHook = layoutHook'
              , manageHook = placeHook myPlacement <+> manageHook defaultConfig <+> manageHook'
              }

-- Hooks ----------------------------------------------------------------------

-- Managehook
manageHook' :: ManageHook
manageHook' = composeAll . concat $
    [ [className =? c --> doFloat                           | c <- myFloats]
    , [title     =? t --> doFloat                           | t <- myOtherFloats]
    , [title     =? t --> doIgnore                          | t <- myIgnores]
    , [className =? c --> doF (W.shift (workspaces' !! 1))  | c <- webApps]
    , [className =? c --> doF (W.shift (workspaces' !! 4))  | c <- graApps]
    , [className =? c --> doF (W.shift (workspaces' !! 3))  | c <- docApps]
    ]
    where
    myFloats       = ["feh", "file-roller", "File-roller", "MPlayer", "Gnome-calculator"]
    myOtherFloats  = ["alsamixer", "file-roller", "Gran Paradiso Preferences", "mailbox", "New Layer", "Color Balance", "Hue-Saturation" ]
    myIgnores      = [""]
    webApps        = ["Gran Paradiso", "Opera", "Navigator", "Shiretoko"]
    graApps        = ["gimp-2.6", "Gimp-2.6", "GIMP", "gimp"]
    docApps        = ["evince", "Evince", "Apvlv"]

-- Placehook
myPlacement = smart (0.5,0.5)

-- Loghook
logHook' :: Handle ->  X ()
logHook' h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }

-- Layouthook
layoutHook' = customLayout

-- Looks ----------------------------------------------------------------------

-- dzen2 settings --
myStatusBar = "/usr/bin/dzen2 -ta l -h 20 -w 1000 -fn '-*-liberation mono-*-r-*-*-10-*-*-*-*-*-*-*' -e '' "

-- XMonad dzen2 bar
customPP :: PP
customPP = defaultPP { ppCurrent = dzenColor "#B8BCB8" "#990000" . pad
                     , ppHidden = dzenColor "#B8BCB8" "#484C48" . pad
                     , ppTitle = dzenColor "#C4C4C4" "" . shorten 120
                     , ppLayout = dzenColor "#990000" "" .
                        (\ x -> fill (case x of
                            "ResizableTall"             -> icon "tall.xbm"
                            "Mirror ResizableTall"      -> icon "mtall.xbm"
                            "Full"                      -> icon "full.xbm"
                            "IM Grid"                   -> icon "mail.xbm"
                            "IM ReflectX IM Full"       -> icon "scorpio.xbm"
                            "TabBar Tall"               -> icon "info_01.xbm"
                            "Tabbed Bottom Simplest"    -> icon "info_02.xbm"
                            _                           -> pad x) 4)
                     , ppWsSep = ""
                     , ppHiddenNoWindows = dzenColor "#616161" "" . pad
                     , ppUrgent = dzenColor "#616161" "#D4D455" . dzenStrip
                     }
                     where
                     icon h = "^i(/home/myrkiada/.xmonad/icons/" ++ h ++ ")"
                     fill :: String -> Int -> String
                     fill h i ="^p(" ++ show i ++ ")" ++ h ++ "^p(" ++ show i ++")"

-- XPConfig
myXPConfig = defaultXPConfig   { bgColor = "#101010"
                               , fgColor = "#990000"
                               , bgHLight = "#990000"
                               , fgHLight = "#B8BCB8"
                               , borderColor = "#990000"
                               , promptBorderWidth = 0
                               , position = Bottom
                               , height = 16
--                               , showCompletionOnTab = True
                               }



-- TabConfig
myTabConfig = defaultTheme { inactiveBorderColor = "#303030"
			               , activeBorderColor = "#303030"
                           , activeColor = "#990000"
                           , inactiveColor = "#990000"
                           , activeTextColor = "#FFFFFF"
                           , inactiveTextColor = "#C4C4C4"
			               , decoHeight = 16
                           }


-- Borders
borderWidth' :: Dimension
borderWidth' = 1

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#000000"
focusedBorderColor' = "#990000"

-- Workspaces
workspaces' :: [WorkspaceId]
workspaces' = clickable $ ["^i(/home/myrkiada/.xmonad/icons/sm4tik/screen.xbm)", -- 0
                           "^i(/home/myrkiada/.xmonad/icons/sm4tik/fox.xbm):web", -- 1
                           "^i(/home/myrkiada/.xmonad/icons/sm4tik/dev.xbm):dev", -- 2
                           "^i(/home/myrkiada/.xmonad/icons/sm4tik/book.xbm):lib", -- 3
                           "^i(/home/myrkiada/.xmonad/icons/sm4tik/mouse_01.xbm):gfx", -- 4
                           "^i(/home/myrkiada/.xmonad/icons/sm4tik/cpu.xbm):vm", -- 5
                           "^i(/home/myrkiada/.xmonad/icons/sm4tik/info_03.xbm):sys" --6
                          ]

              where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                                    (i,ws) <- zip [1..] l,
                                    let n = if i == 10 then 0 else i ]



-- Layouts --------------------------------------------------------------------

customLayout = avoidStruts . smartBorders. toggleLayouts Full $

    -- Layout for Workspaces
    onWorkspace (workspaces' !! 0) (tiled ||| (Mirror tiled) ||| Full ||| tallDefault shrinkText myTabConfig) $
    onWorkspace (workspaces' !! 4) (gimp ||| gimpg) $
    onWorkspace (workspaces' !! 6) ((Mirror tiled) ||| tiled) $

    -- Default layout
    tiled ||| (Mirror tiled)  ||| tallDefault shrinkText myTabConfig ||| tabbedBottomAlways shrinkText myTabConfig

  where
    tiled  = ResizableTall 1 (2/100) (1/2) []
    mirror = (Mirror tiled)
    gimpw  = tabbedBottomAlways shrinkText myTabConfig
    gimpg  = reflectHoriz $
             withIM (0.18) (Role "gimp-toolbox") mirror
    gimp   = reflectHoriz $
             withIM (0.18) (Role "gimp-toolbox") gimpw

-------------------------------------------------------------------------------
-- Terminal --
terminal' :: String
terminal' = "urxvt"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' :: KeyMask
modMask' = mod4Mask

-- keys
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask,               xK_F1    ), spawn "firefox")
    , ((modMask,               xK_F2    ), spawn "claws-mail")
    , ((modMask,               xK_F4    ), spawn "thunar")
    , ((modMask,               xK_F12   ), spawn "gnome-calculator")
    , ((modMask,               xK_F3    ), spawn (terminal'++" -e htop"))
    , ((modMask,               xK_F9    ), spawn "scrot")

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,               xK_b     ), sendMessage ToggleStruts)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- focus
    , ((modMask,               xK_Right ), windows W.focusDown)
    , ((modMask,               xK_Left  ), windows W.focusUp)
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- workspaces
    , ((modMask .|. controlMask, xK_Right), nextScreen)
    , ((modMask .|. controlMask, xK_Left ), prevScreen)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)
    , ((modMask,               xK_f     ), sendMessage ToggleStruts >> sendMessage ToggleLayout)

    -- volume management
    , ((modMask,               xK_KP_Add       ), spawn "amixer set PCM 1+")
    , ((modMask,               xK_KP_Subtract  ), spawn "amixer set PCM 1-")
    , ((modMask,	           xK_KP_Multiply  ), spawn "amixer set PCM toggle")

    -- moc controls
    , ((modMask,	           xK_KP_Right     ), spawn "mpc next")
    , ((modMask,               xK_KP_Left      ), spawn "mpc prev")
    , ((modMask,	           xK_KP_Insert    ), spawn "mpc toggle")
    , ((modMask,	           xK_KP_Delete    ), spawn "mpc stop")

    -- prompts
    , ((modMask,  xK_p     ), shellPrompt myXPConfig)
--    , ((modMask,  xK_n	   ), appendFilePrompt myXPConfig "/home/myrkiada/NOTES")
    , ((modMask,  xK_m     ), layoutPrompt myXPConfig)

    -- GridSelect
    , ((modMask,  xK_g     ), goToSelected defaultGSConfig)

    -- lock, quit, or restart
    , ((modMask .|. shiftMask, xK_x     ), spawn "xlock")
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------
