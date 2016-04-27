{-| Contains miscellaneous utility functions and the main
    functions for interfacing with the engine. -}
module FRP.Helm (
  -- * Types
  Time,
  EngineConfig(..),
  -- * Engine
  runAndQuitOnSignal,
  run,
  defaultConfig,
  -- * Prelude
  module Color,
  module Graphics,
  module Utilities,
  module Signal,
  FRP.Helm.Signal.lift
) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Bits
import Data.Foldable (forM_)
import qualified Data.Text as T
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import FRP.Elerea.Param hiding (Signal)
import FRP.Helm.Color as Color
import FRP.Helm.Engine
import FRP.Helm.Graphics as Graphics
import FRP.Helm.Utilities as Utilities
import FRP.Helm.Sample
import FRP.Helm.Signal as Signal hiding (lift)
import qualified FRP.Helm.Signal (lift)
import FRP.Helm.Time (Time)
import qualified FRP.Helm.Window as Window
import System.FilePath
import System.Endian
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.Cairo as Cairo

type Helm a = StateT Engine Cairo.Render a

{-| A data structure holding the main element and information required for
    rendering. -}
data Application = Application {
  mainElement    :: Element,
  mainDimensions :: (Int, Int),
  mainContinue   :: Bool
}

{-| A data structure describing miscellaneous initial configurations of the
    game window and engine. -}
data EngineConfig = EngineConfig {
  windowDimensions :: (Int, Int),
  windowIsFullscreen :: Bool,
  windowIsResizable :: Bool,
  windowTitle :: String
}

{-| Creates the default configuration for the engine. You should change the
    fields where necessary before passing it to 'run'. -}
defaultConfig :: EngineConfig
defaultConfig = EngineConfig {
  windowDimensions = (800, 600),
  windowIsFullscreen = False,
  windowIsResizable = True,
  windowTitle = ""
}

{-| Creates a new engine that can be run later using 'run'. -}
startup :: EngineConfig -> IO Engine
startup (EngineConfig { .. }) = do
    window <- SDL.createWindow windowTitle (SDL.Position 0 0)
                (SDL.Size (fromIntegral w) (fromIntegral h)) wflags
    renderer <- SDL.createRenderer window (SDL.Device (-1)) rflags

    return Engine { window   = window
                  , renderer = renderer
                  , cache    = Map.empty
                  , continue = True
                  }

  where
    optWhen :: Bool -> a -> [a]
    optWhen b a = if b then [a] else []
    (w, h) = windowDimensions
    wflags = SDL.WindowShown:(optWhen windowIsResizable  SDL.WindowResizable ++
                              optWhen windowIsFullscreen SDL.WindowFullscreen)
    rflags = [SDL.PresentVSync, SDL.Accelerated]


{-| A more generic version of @run@ that allows you to provide a signal
    that when true, will quit
-}
runAndQuitOnSignal :: EngineConfig -> Signal Bool -> Signal Element -> IO ()
runAndQuitOnSignal config quitOn element  = do
  engine <- startup config
  run_ engine $ application <~ element
                            ~~ Window.dimensions
                            ~~ ((&&) <$> (not <$> quitOn) <*> continue')
                            ~~ exposed
  where
    application :: Element -> (Int, Int) -> Bool -> () -> Application
    application e d c _ = Application e d c
    run_ eng (Signal gen) = (start gen >>= run' eng) `finally` SDL.quit

{-| Initializes and runs the game engine. The supplied signal generator is
    constantly sampled for an element to render until the user quits.

    > import FRP.Helm
    > import qualified FRP.Helm.Window as Window
    >
    > render :: (Int, Int) -> Element
    > render (w, h) = collage w h [rect (fromIntegral w) (fromIntegral h) |> filled red]
    >
    > main :: IO ()
    > main = run defaultConfig $ lift render Window.dimensions
 -}
run :: EngineConfig -> Signal Element -> IO ()
run config element = runAndQuitOnSignal config (constant False) element

{-| An event that triggers when SDL thinks we need to re-draw. -}
exposed :: Signal ()
exposed = Signal getExposed
  where
    getExposed = effectful $ do
      -- FIXME: Expose this in hsSDL2
--      SDL.pumpEvents
      mbEvent <- SDL.pollEvent
      return $ case mbEvent of
        Just (SDL.Event { SDL.eventData = SDL.Window { SDL.windowEvent = e }})  -> Changed ()
        _ -> Unchanged ()

{-| An event that triggers when SDL thinks we need to quit. -}
quit :: Signal ()
quit = Signal getQuit
  where
    getQuit = effectful $ do
      mbEvent <- SDL.pollEvent
      return $ case mbEvent of
        Just (SDL.Event { SDL.eventData = SDL.Quit }) -> Changed ()
        _ -> Unchanged ()

continue' :: Signal Bool
continue' = (==0) <~ count quit

{-| A utility function called by 'run' that samples the element
    or quits the entire engine if SDL events say to do so. -}
run' :: Engine -> (Engine -> IO (Sample Application)) -> IO ()
run' engine smp = when (continue engine) $ smp engine >>= renderIfChanged engine
                                                      >>= flip run' smp

{-| Renders when the sample is marked as changed delays the thread otherwise -}
renderIfChanged :: Engine -> Sample Application -> IO Engine
renderIfChanged engine event =  case event of
    Changed   app -> if mainContinue app
                     then render engine (mainElement app) (mainDimensions app)
                     else return engine { continue = False }

    Unchanged _ -> do threadDelay 1000
                      return engine

{-| A utility function that renders a previously sampled element
    using an engine state. -}
render :: Engine -> Element -> (Int, Int) -> IO Engine
render engine@(Engine { .. }) element (w, h) = do
  texture <- SDL.createTexture renderer SDL.PixelFormatARGB8888
               SDL.TextureAccessStreaming (fromIntegral w) (fromIntegral h)

  res <- SDL.lockTexture texture Nothing $ \(pixels, pitch) -> do
    Cairo.withImageSurfaceForData (castPtr pixels)
           Cairo.FormatARGB32 w h pitch $ \surface -> Cairo.renderWith surface
             $ evalStateT (render' w h element) engine

  SDL.unlockTexture texture

  SDL.renderClear renderer
  SDL.renderCopy renderer texture Nothing Nothing
  SDL.destroyTexture texture
  SDL.renderPresent renderer

  return res


{-| A utility function called by 'render' that is called by Cairo
    when it's ready to do rendering. -}
render' :: Int -> Int -> Element -> Helm Engine
render' w h element = do
  lift $ do Cairo.setSourceRGB 0 0 0
            Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
            Cairo.fill

  renderElement element
  get

{-| A utility function that lazily grabs an image surface from the cache,
    i.e. creating it if it's not already stored in it. -}
getSurface :: FilePath -> Helm (Cairo.Surface, Int, Int)
getSurface src = do
  Engine _ _ cache _ <- get

  case Map.lookup src cache of
    Just surface -> do
      w <- Cairo.imageSurfaceGetWidth surface
      h <- Cairo.imageSurfaceGetHeight surface

      return (surface, w, h)

    Nothing -> do
      -- TODO: Use SDL_image to support more formats. I gave up after it was painful
      -- to convert between the two surface types safely.
      -- FIXME: Does this throw an error?
      surface <- liftIO $ Cairo.imageSurfaceCreateFromPNG src
      w <- liftIO $ Cairo.imageSurfaceGetWidth surface
      h <- liftIO $ Cairo.imageSurfaceGetHeight surface

      modify (\engine -> engine{cache=Map.insert src surface cache})
      return (surface, w, h)

{-| A utility function for rendering a specific element. -}
renderElement :: Element -> Helm ()
renderElement (CollageElement w h center forms) = do
  lift $ do Cairo.save
            Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
            Cairo.clip
            forM_ center $ uncurry Cairo.translate
  mapM_ renderForm forms
  lift Cairo.restore

renderElement (ImageElement (sx, sy) sw sh src stretch) = do
  (surface, w, h) <- getSurface (normalise src)

  lift $ do Cairo.save
            Cairo.translate (-fromIntegral sx) (-fromIntegral sy)

            if stretch then
              Cairo.scale (fromIntegral sw / fromIntegral w)
                (fromIntegral sh / fromIntegral h)
            else
              Cairo.scale 1 1

            Cairo.setSourceSurface surface 0 0
            Cairo.translate (fromIntegral sx) (fromIntegral sy)
            Cairo.rectangle 0 0 (fromIntegral sw) (fromIntegral sh)
            if stretch then
                Cairo.paint
            else
                Cairo.fill

            Cairo.restore

renderElement (TextElement (Text { textColor = (Color r g b a), .. })) =
   error "no support for rendering text elements"
  where
    i = 0
    j = length textUTF8

{-| A utility function that goes into a state of transformation and then pops
    it when finished. -}
withTransform :: Double -> Double -> Double -> Double -> Helm () -> Helm ()
withTransform s t x y f = do
  lift $ Cairo.save >> Cairo.scale s s >> Cairo.translate x y >> Cairo.rotate t
  f
  lift Cairo.restore

{-| A utility function that sets the Cairo line cap based off of our version. -}
setLineCap :: LineCap -> Cairo.Render ()
setLineCap cap = case cap of
  FlatCap   -> Cairo.setLineCap Cairo.LineCapButt
  RoundCap  -> Cairo.setLineCap Cairo.LineCapRound
  PaddedCap -> Cairo.setLineCap Cairo.LineCapSquare

{-| A utility function that sets the Cairo line style based off of our version. -}
setLineJoin :: LineJoin -> Cairo.Render ()
setLineJoin join = case join of
  SmoothJoin    -> Cairo.setLineJoin Cairo.LineJoinRound
  SharpJoin lim -> Cairo.setLineJoin Cairo.LineJoinMiter >> Cairo.setMiterLimit lim
  ClippedJoin   -> Cairo.setLineJoin Cairo.LineJoinBevel

{-| A utility function that sets up all the necessary settings with Cairo
    to render with a line style and then strokes afterwards. Assumes
    that all drawing paths have already been setup before being called. -}
setLineStyle :: LineStyle -> Cairo.Render ()
setLineStyle (LineStyle { lineColor = Color r g b a, .. }) = do
  Cairo.setSourceRGBA r g b a
  setLineCap lineCap
  setLineJoin lineJoin
  Cairo.setLineWidth lineWidth
  Cairo.setDash lineDashing lineDashOffset
  Cairo.stroke

{-| A utility function that sets up all the necessary settings with Cairo
    to render with a fill style and then fills afterwards. Assumes
    that all drawing paths have already been setup before being called. -}
setFillStyle :: FillStyle -> Helm ()
setFillStyle (Solid (Color r g b a)) = lift $ do
  Cairo.setSourceRGBA r g b a
  Cairo.fill

setFillStyle (Texture src) = do
  (surface, _, _) <- getSurface (normalise src)
  lift $ do Cairo.setSourceSurface surface 0 0
            Cairo.getSource >>= flip Cairo.patternSetExtend Cairo.ExtendRepeat
            Cairo.fill

setFillStyle (Gradient (Linear (sx, sy) (ex, ey) points)) =
  lift $ Cairo.withLinearPattern sx sy ex ey
       $ \pattern -> setFillStyle' pattern points

setFillStyle (Gradient (Radial (sx, sy) sr (ex, ey) er points)) =
  lift $ Cairo.withRadialPattern sx sy sr ex ey er
       $ \pattern -> setFillStyle' pattern points

{-| A utility function that adds color stops to a pattern and then fills it. -}
setFillStyle' :: Cairo.Pattern -> [(Double, Color)] -> Cairo.Render ()
setFillStyle' pattern points = do
  Cairo.setSource pattern
  mapM_ (\(o, Color r g b a) -> Cairo.patternAddColorStopRGBA pattern o r g b a) points
  Cairo.fill

{-| A utility that renders a form. -}
renderForm :: Form -> Helm ()
renderForm Form { .. } = withTransform formScale formTheta formX formY $
  case formStyle of
    PathForm style ~ps @ ((hx, hy) : _) -> lift $ do
      Cairo.newPath
      Cairo.moveTo hx hy
      mapM_ (uncurry Cairo.lineTo) ps
      setLineStyle style

    ShapeForm style shape -> do
      lift Cairo.newPath

      case shape of
        PolygonShape ~ps @ ((hx, hy) : _) ->
          lift $ do Cairo.moveTo hx hy
                    mapM_ (uncurry Cairo.lineTo) ps

        RectangleShape (w, h) -> lift $ Cairo.rectangle (-w / 2) (-h / 2) w h

        ArcShape (cx, cy) a1 a2 r (sx, sy) ->
          lift $ do Cairo.scale sx sy
                    Cairo.arc cx cy r a1 a2
                    Cairo.scale 1 1

      either (lift . setLineStyle) setFillStyle style

    ElementForm element -> renderElement element
    GroupForm mayhaps forms -> do
      lift $ do Cairo.save
                forM_ mayhaps Cairo.setMatrix
      mapM_ renderForm forms
      lift Cairo.restore
