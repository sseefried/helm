{-| Contains signals that sample input from the mouse. -}
module FRP.Helm.Touch
(
  -- * Types
  Touch(..),
  -- * Position
  position

) where

import Control.Applicative (pure)
import Data.Bits
import Data.Maybe
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import FRP.Elerea.Param hiding (Signal)
import FRP.Helm.Sample
import FRP.Helm.Signal
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Events as SDL


{-| A data structure describing a touch -}
type FingerId = Int
data Touch = Touch FingerId Float Float
  deriving (Show, Eq, Ord, Read)

toTouch :: SDL.Finger -> Maybe Touch
toTouch m = case m of
  SDL.Finger {..} -> Just (Touch (fromIntegral fingerId)
                                 (realToFrac fingerX)
                                 (realToFrac fingerY))
  _                    -> Nothing


{-| The current position of the touch in normalized co-ordinates -}
position :: FingerId -> Signal (Maybe (Float, Float))
position fid = Signal $ getPosition >>= transfer (pure Nothing) update
  where
    getPosition = effectful $ do
      mbFinger <- SDL.getTouchFinger (fromIntegral fid)
      case mbFinger of
        Nothing -> return Nothing
        Just f -> return $ Just ( realToFrac (SDL.fingerX f)
                                , realToFrac (SDL.fingerY f))