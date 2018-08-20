import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Actions.CycleWS
import XMonad.Layout.Reflect
import XMonad.Actions.RotSlaves
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.SimpleFloat


-- n.b. the following code is inlined & badly ported from XMonad.Config.Gnome
-- from the XMonad Contrib packages:

-- original code by:
--      Copyright    : (c) Spencer Janssen <spencerjanssen@gmail.com>
--      License      : BSD
-- ported from GNOME to MATE by:
--      Copyright    : (c) rfc <reuben.fletchercostin@gmail.com>
--      License      : BSD
-- customizations for my preferences by:
--      Copyright    : (c) alexandrelaplante
--      License      : BSD

import XMonad.Config.Desktop
import XMonad.Util.Run (safeSpawn)

import qualified Data.Map as M

import System.Environment (getEnvironment)

-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Mate
-- >
-- > main = xmonad mateConfig
--
-- For examples of how to further customize @mateConfig@ see "XMonad.Config.Desktop".

mateConfig = desktopConfig
    { terminal = "mate-terminal --hide-menubar"
    , modMask = mod4Mask
    , keys = myKeys
    , manageHook = mateManageHook <+> manageHook desktopConfig
    , workspaces = ["1","2","3","4","5","6"]
    , layoutHook =  myLayout
    --, handleEventHook = fullscreenEventHook
    , startupHook = mateRegister >> startupHook desktopConfig }

--myLayout = htiled ||| fullscreen ||| vtiled ||| simpleFloat
myLayout = vtiled ||| fullscreen ||| htiled
    where
        vtiled = noBorders ( desktopLayoutModifiers ( reflectHoriz (ResizableTall 1 (3/100) ratio [1,2*ratio] ) ) )
        htiled = noBorders ( desktopLayoutModifiers ( Mirror (ResizableTall 1 (3/100) ratio [] ) ) )
        fullscreen = noBorders ( Full )

        ratio   = toRational (2/(1+sqrt(5)::Double)) -- golden

addKeys (XConfig {modMask = modm}) =
    [
      ((modm, xK_p), mateRun)
    , ((modm .|. shiftMask, xK_q), spawn "mate-session-save --kill")
	, ((modm .|. shiftMask, xK_x), kill)
    , ((modm, xK_z), sendMessage NextLayout)
    --, ((modm, xK_Down), sendMessage NextLayout)
        --resizing
    , ((modm,               xK_l), sendMessage Shrink)
    , ((modm,               xK_h), sendMessage Expand)
    , ((modm .|. shiftMask, xK_l), sendMessage MirrorShrink)
    , ((modm .|. shiftMask, xK_h), sendMessage MirrorExpand)
    , ((modm .|. shiftMask, xK_Down), sendMessage Shrink)
    , ((modm .|. shiftMask, xK_Up), sendMessage Expand)
        --moving windows
    , ((modm,               xK_k), rotAllUp)
    , ((modm,               xK_j), rotAllDown)
    , ((modm .|. shiftMask, xK_k), rotSlavesUp)
    , ((modm .|. shiftMask, xK_j), rotSlavesDown)
    , ((modm              , xK_comma), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_period ), sendMessage (IncMasterN 1))
    , ((modm,               xK_Up), rotAllUp)
    , ((modm,               xK_Down), rotAllDown)
        -- moving workspaces
    , ((modm,               xK_Right), moveTo Next HiddenWS)
    , ((modm,               xK_Left),  moveTo Prev HiddenWS)
    , ((modm .|. shiftMask, xK_Right), shiftTo Next HiddenWS >> moveTo Next HiddenWS)
    , ((modm .|. shiftMask, xK_Left),  shiftTo Prev HiddenWS >> moveTo Prev HiddenWS)
    , ((modm              , xK_a), swapNextScreen)
    , ((modm .|. shiftMask, xK_a), shiftNextScreen)
    ]

removeKeys XConfig{modMask = modm} =
    [
        -- unused
          (modm, xK_space)
        , (modm, xK_c)
    ]

defKeys   = keys desktopConfig
delKeys x = foldr M.delete           (defKeys x) (removeKeys x)
myKeys x  = foldr (uncurry M.insert) (delKeys x) (addKeys    x)

mateManageHook = composeAll . concat $ [
        [ className =? "Do" --> doIgnore ],
        [ className =? "" --> doFloat ]
    ]

-- | Launch the "Run Application" dialog.  mate-panel must be running for this
-- to work.
mateRun :: X ()
mateRun = withDisplay $ \dpy -> do
    rw <- asks theRoot
    mate_panel <- getAtom "_MATE_PANEL_ACTION"
    panel_run   <- getAtom "_MATE_PANEL_ACTION_RUN_DIALOG"

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw mate_panel 32 panel_run 0
        sendEvent dpy rw False structureNotifyMask e
        sync dpy False

-- | Register xmonad with mate. 'dbus-send' must be in the $PATH with which
-- xmonad is started.
--
-- This action reduces a delay on startup only only if you have configured
-- mate-session>=2.26: to start xmonad with a command as such:
--
-- > mateconftool-2 -s /desktop/mate/session/required_components/windowmanager xmonad --type string
mateRegister :: MonadIO m => m ()
mateRegister = io $ do
    x <- lookup "DESKTOP_AUTOSTART_ID" `fmap` getEnvironment
    whenJust x $ \sessionId -> safeSpawn "dbus-send"
            ["--session"
            ,"--print-reply=string"
            ,"--dest=org.mate.SessionManager"
            ,"/org/mate/SessionManager"
            ,"org.mate.SessionManager.RegisterClient"
            ,"string:xmonad"
            ,"string:"++sessionId]

-- end of inlined mateConfig

-- here we actually configure xmonad
main = xmonad mateConfig
