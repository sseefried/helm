{-| Contains signals that sample input from the mouse. -}
module FRP.Helm.Touch
(
  -- * Types
  Touch(..),
  -- * Position
  position

) where

import Control.Applicative
import Data.Bits
import Data.Maybe
import Foreign.C.Types -- Remove
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import FRP.Elerea.Param hiding (Signal)
import FRP.Helm.Sample
import FRP.Helm.Signal
import qualified Graphics.UI.SDL as SDL hiding (getTouchFinger)
import qualified Graphics.UI.SDL.Events as SDL


{-| A data structure describing a touch -}
type FingerId = Int
data Touch = Touch FingerId Float Float
  deriving (Show, Eq, Ord, Read)

toTouch :: Finger -> Maybe Touch
toTouch m = case m of
  Finger {..} -> Just (Touch (fromIntegral fingerId)
                                 (realToFrac fingerX)
                                 (realToFrac fingerY))
  _                    -> Nothing


{-| The current position of the touch in normalized co-ordinates -}
position :: FingerId -> Signal (Maybe (Float, Float))
position fid = Signal $ getPosition >>= transfer (pure Nothing) update
  where
    getPosition = effectful $ do
      mbFinger <- getTouchFinger (fromIntegral fid)
      case mbFinger of
        Nothing -> return Nothing
        Just f -> return $ Just ( realToFrac (fingerX f)
                                , realToFrac (fingerY f))

--
-- FIXME: You gotta remove this!
--
data Finger = Finger { fingerId       :: CLong
                     , fingerX        :: CFloat
                     , fingerY        :: CFloat
                     , fingerPressure :: CFloat
                     } deriving (Eq, Show)

peekFinger :: Ptr Finger -> IO Finger
peekFinger ptr = do
      Finger
  <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
  <*> (\hsc_ptr -> peekByteOff hsc_ptr 8)  ptr
  <*> (\hsc_ptr -> peekByteOff hsc_ptr 12)  ptr
  <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr


foreign import ccall "SDL_GetTouchFinger" sdlGetTouchFinger :: CLong -> CInt -> IO (Ptr Finger)
foreign import ccall "SDL_GetTouchDevice" sdlGetTouchDevice :: CInt -> IO CLong

getTouchFinger :: CInt -> IO (Maybe Finger)
getTouchFinger fid = do
   touchDevIndex <- sdlGetTouchDevice 0
   ptr <- sdlGetTouchFinger touchDevIndex fid
   if ptr == nullPtr
    then return Nothing
    else Just <$> peekFinger ptr
