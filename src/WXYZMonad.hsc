{-# LANGUAGE CApiFFI #-}

module WXYZMonad
    ( WXYZ(..)
    , WXYZConf(..)
    , WXYZState(..)
    , Dimension
    , Layout(..)
    , LayoutClass(..)
    , Message
    , Position
    , Rectangle(..)
    , Window(..)
    , WindowSet
    , LayoutMessages(..)
    , ScreenDetail(..)
    , SomeMessage(..)
    , WorkspaceId
    , catchWXYZ
    , io
    , fromMessage
    , runOnWorkspaces
    , runWXYZ
    , withWindowSet
    ) where

import           Control.Exception (fromException, throw)
import qualified Control.Exception as E
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.State.Class ()
import           Data.Int
import qualified Data.Map as M
import           Data.Typeable
import           Data.Word
import           Foreign.Ptr
import           System.Exit (ExitCode)
import           System.IO (hPrint, stderr)

import           Event
import           Key
import           StackSet hiding (modify)

#include "clib.h"

---------------
-- Useful types

type Position  = Int32
type Dimension = Word32

data Rectangle = Rectangle {
            rect_x      :: !Position,
            rect_y      :: !Position,
            rect_width  :: !Dimension,
            rect_height :: !Dimension
        }
    deriving (Eq,Show,Read)


data Layer = Background | Bottom | Top | Overlay
    deriving (Eq, Ord)
data Window = TopLevel (Ptr CXdgTopLevel)
            | LayerSurface (Ptr CLayerSurface) Layer
    deriving (Eq, Ord)

type WindowSet = StackSet WorkspaceId (Layout Window) Window ScreenId ScreenDetail
type WindowSpace = Workspace WorkspaceId (Layout Window) Window

-- | Virtual workspace indices
type WorkspaceId = String

-- | Physical screen indices
newtype ScreenId    = S Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

-- | The 'Rectangle' with screen dimensions
newtype ScreenDetail = SD { screenRect :: Rectangle }
    deriving (Eq,Show, Read)

type LayerSurface = Ptr CLayerSurface

---------------------------
-- Our window manager monad

data WXYZState = State {
    windowset :: WindowSet
}

data WXYZConf = Config {
    keyBindings :: M.Map (Modifier,KeySym) (WXYZ ()),

    layoutHook     :: Layout Window,

    startupHook :: WXYZ (),
    workspaces :: ![String] -- ^ The list of workspaces' names
}

newtype WXYZ a = WXYZ (ReaderT WXYZConf (StateT WXYZState IO) a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState WXYZState, MonadReader WXYZConf)

runWXYZ :: WXYZConf -> WXYZState -> WXYZ a -> IO (a, WXYZState)
runWXYZ c st (WXYZ a) = runStateT (runReaderT a c) st

-- | Run in the 'WXYZ' monad, and in case of exception, and catch it and log it
-- to stderr, and run the error case.
catchWXYZ :: WXYZ a -> WXYZ a -> WXYZ a
catchWXYZ job errcase = do
    st <- get
    c <- ask
    (a, s') <- io $ runWXYZ c st job `E.catch` \e -> case fromException e of
                        Just (_ :: ExitCode) -> throw e
                        _ -> do hPrint stderr e; runWXYZ c st errcase
    put s'
    return a

------------------------------------------------------------------------
-- LayoutClass handling. See particular instances in Operations.hs

-- | An existential type that can hold any object that is in 'Read'
--   and 'LayoutClass'.
data Layout a = forall l. (LayoutClass l a, Read (l a)) => Layout (l a)

-- | Every layout must be an instance of 'LayoutClass', which defines
-- the basic layout operations along with a sensible default for each.
--
-- All of the methods have default implementations, so there is no
-- minimal complete definition.  They do, however, have a dependency
-- structure by default; this is something to be aware of should you
-- choose to implement one of these methods.  Here is how a minimal
-- complete definition would look like if we did not provide any default
-- implementations:
--
-- * 'runLayout' || (('doLayout' || 'pureLayout') && 'emptyLayout')
--
-- * 'handleMessage' || 'pureMessage'
--
-- * 'description'
--
-- Note that any code which /uses/ 'LayoutClass' methods should only
-- ever call 'runLayout', 'handleMessage', and 'description'!  In
-- other words, the only calls to 'doLayout', 'pureMessage', and other
-- such methods should be from the default implementations of
-- 'runLayout', 'handleMessage', and so on.  This ensures that the
-- proper methods will be used, regardless of the particular methods
-- that any 'LayoutClass' instance chooses to define.
class (Show (layout a), Typeable layout) => LayoutClass layout a where

    -- | By default, 'runLayout' calls 'doLayout' if there are any
    --   windows to be laid out, and 'emptyLayout' otherwise.  Most
    --   instances of 'LayoutClass' probably do not need to implement
    --   'runLayout'; it is only useful for layouts which wish to make
    --   use of more of the 'Workspace' information (for example,
    --   "XMonad.Layout.PerWorkspace").
    runLayout :: Workspace WorkspaceId (layout a) a
              -> Rectangle
              -> WXYZ ([(a, Rectangle)], Maybe (layout a))
    runLayout (Workspace _ l ms) r = maybe (emptyLayout l r) (doLayout l r) ms

    -- | Given a 'Rectangle' in which to place the windows, and a 'Stack'
    -- of windows, return a list of windows and their corresponding
    -- Rectangles.  If an element is not given a Rectangle by
    -- 'doLayout', then it is not shown on screen.  The order of
    -- windows in this list should be the desired stacking order.
    --
    -- Also possibly return a modified layout (by returning @Just
    -- newLayout@), if this layout needs to be modified (e.g. if it
    -- keeps track of some sort of state).  Return @Nothing@ if the
    -- layout does not need to be modified.
    --
    -- Layouts which do not need access to the 'WXYZ' monad ('IO', window
    -- manager state, or configuration) and do not keep track of their
    -- own state should implement 'pureLayout' instead of 'doLayout'.
    doLayout    :: layout a -> Rectangle -> Stack a
                -> WXYZ ([(a, Rectangle)], Maybe (layout a))
    doLayout l r s   = return (pureLayout l r s, Nothing)

    -- | This is a pure version of 'doLayout', for cases where we
    -- don't need access to the 'WXYZ' monad to determine how to lay out
    -- the windows, and we don't need to modify the layout itself.
    pureLayout  :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
    pureLayout _ r s = [(focus s, r)]

    -- | 'emptyLayout' is called when there are no windows.
    emptyLayout :: layout a -> Rectangle -> WXYZ ([(a, Rectangle)], Maybe (layout a))
    emptyLayout _ _ = return ([], Nothing)

    -- | 'handleMessage' performs message handling.  If
    -- 'handleMessage' returns @Nothing@, then the layout did not
    -- respond to the message and the screen is not refreshed.
    -- Otherwise, 'handleMessage' returns an updated layout and the
    -- screen is refreshed.
    --
    -- Layouts which do not need access to the 'WXYZ' monad to decide how
    -- to handle messages should implement 'pureMessage' instead of
    -- 'handleMessage' (this restricts the risk of error, and makes
    -- testing much easier).
    handleMessage :: layout a -> SomeMessage -> WXYZ (Maybe (layout a))
    handleMessage l  = return . pureMessage l

    -- | Respond to a message by (possibly) changing our layout, but
    -- taking no other action.  If the layout changes, the screen will
    -- be refreshed.
    pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
    pureMessage _ _  = Nothing

    -- | This should be a human-readable string that is used when
    -- selecting layouts by name.  The default implementation is
    -- 'show', which is in some cases a poor default.
    description :: layout a -> String
    description      = show

instance LayoutClass Layout a where
    runLayout (Workspace i (Layout l) ms) r = fmap (fmap Layout) `fmap` runLayout (Workspace i l ms) r
    doLayout (Layout l) r s  = fmap (fmap Layout) `fmap` doLayout l r s
    emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
    handleMessage (Layout l) = fmap (fmap Layout) . handleMessage l
    description (Layout l)   = description l

instance Show (Layout a) where show (Layout l) = show l

-- | Based on ideas in /An Extensible Dynamically-Typed Hierarchy of
-- Exceptions/, Simon Marlow, 2006. Use extensible messages to the
-- 'handleMessage' handler.
--
-- User-extensible messages must be a member of this class.
--
class Typeable a => Message a

-- |
-- A wrapped value of some type in the 'Message' class.
--
data SomeMessage = forall a. Message a => SomeMessage a

-- |
-- And now, unwrap a given, unknown 'Message' type, performing a (dynamic)
-- type check on the result.
--
fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- | 'LayoutMessages' are core messages that all layouts (especially stateful
-- layouts) should consider handling.
data LayoutMessages = Hide              -- ^ sent when a layout becomes non-visible
                    | ReleaseResources  -- ^ sent when xmonad is exiting or restarting
    deriving Eq

instance Message LayoutMessages


-- ---------------------------------------------------------------------
-- Convenient wrappers to state

-- | Run a monadic action with the current stack set
withWindowSet :: (WindowSet -> WXYZ a) -> WXYZ a
withWindowSet f = gets windowset >>= f

-- ---------------------------------------------------------------------
-- General utilities

-- | Lift an 'IO' action into the 'WXYZ' monad
io :: MonadIO m => IO a -> m a
io = liftIO

-- | This is basically a map function, running a function in the 'WXYZ' monad on
-- each workspace with the output of that function being the modified workspace.
runOnWorkspaces :: (WindowSpace -> WXYZ WindowSpace) -> WXYZ ()
runOnWorkspaces job = do
    ws <- gets windowset
    h <- mapM job $ hidden ws
    c:v <- mapM (\s -> (\w -> s { workspace = w}) <$> job (workspace s))
             $ current ws : visible ws
    modify $ \s -> s { windowset = ws { current = c, visible = v, hidden = h } }

