import qualified Graphics.UI.SDL as SDL
import Data.Word
import Data.Time
import System.CPUTime
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Prelude hiding ((.),id)
import Control.Wire
import FRP.Netwire
import Phys

data Keystate = Keystate {up :: Bool, left :: Bool, right :: Bool, down :: Bool, space :: Bool} deriving (Show,Eq)

run = alloca $ \eventptr -> do
  status <- SDL.pollEvent eventptr
  if status == 1 then do
    event <- peek eventptr
    case event of
      SDL.QuitEvent _ _ -> return False
      _ -> print event >> run
    else
      run
         
run' r s w = alloca $ \eventptr -> do
  (st, nextSession) <- stepSession s
  (Right count, nextWire) <- stepWire w st (Right undefined)
  status <- SDL.pollEvent eventptr
  ks <- SDL.getKeyboardState nullPtr
  keystate <- getKeyStates ks
  if status == 1 then do
    print keystate
    event <- peek eventptr
    case event of
      SDL.QuitEvent _ _ -> return False
      _ -> print keystate >> run' r nextSession nextWire
    else
     run' r nextSession nextWire

getKeyStates :: Ptr Word8 -> IO (Keystate)
getKeyStates ks = do
    let checked_keys = [SDL.scancodeUp, SDL.scancodeLeft, SDL.scancodeRight, SDL.scancodeDown, SDL.scancodeSpace]
    [upstate,leftstate,rightstate,downstate,spacestate] <- mapM (fmap (== 1) . peekElemOff ks . fromIntegral) checked_keys
    return $ Keystate upstate leftstate rightstate downstate spacestate

frameWire :: Float -> Wire (Timed Float ()) e m Float Float
frameWire frames = let newframes = frames + 1.0 in mkSF (\s a -> (newframes, frameWire newframes))

main = do
  SDL.init SDL.initFlagEverything
  windowTitle <- newCString "Hello World"
  window <- SDL.createWindow windowTitle 100 100 640 400 SDL.windowFlagShown
  renderer <- SDL.createRenderer window (fromIntegral (-1)) SDL.rendererFlagPresentVSync
  continue <- run' renderer clockSession_ (integral 0 . pure 1)
  SDL.destroyRenderer(renderer)
  SDL.destroyWindow(window)
  SDL.quit
