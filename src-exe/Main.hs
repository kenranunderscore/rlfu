{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foreign.C.Types (CInt)
import ECS
import Data.Word (Word8, Word32)
import Control.Monad
import Data.Function ((&))
import SDL (($=))
import SDL qualified
import SDL.Vect
import Sdl qualified

data Renderable = Renderable
  { x :: CInt
  , y :: CInt
  , color :: V4 Word8
  , sideLength :: CInt
  }
  deriving stock (Show)

data Components = Components
  { renderables :: ComponentArray Renderable
  }
  deriving stock (Show)

calcFpsInfo :: (Word32, Int) -> IO (Word32, Int)
calcFpsInfo (t, nframes) = do
  ticks <- SDL.ticks
  if t < ticks - 250
    then do
      when (ticks > 250) $ putStrLn $ "FPS: " <> show (4 * nframes)
      pure (ticks, 0)
    else do
      pure (t, nframes + 1)

handleEvents :: [SDL.Event] -> IO Bool
handleEvents evts' = do
  let evts = fmap SDL.eventPayload evts'
  let quit =
        evts
        & fmap
        ( \case
            SDL.KeyboardEvent evt | SDL.keyboardEventKeyMotion evt == SDL.Pressed ->
              case SDL.keysymKeycode (SDL.keyboardEventKeysym evt) of
                SDL.KeycodeEscape -> True
                _ -> False
            SDL.QuitEvent -> True
            _ -> False
                    )
        & or
  pure quit

main :: IO ()
main = do
  let e = fst $ createEntity allEntities
      allComponents = Components $ addComponent e (Renderable 100 200 (V4 100 maxBound 20 maxBound) 30) emptyComponentArray
  Sdl.withSdlEnvironment $ \_win renderer -> do
    let loop comps frameInfo = do
          SDL.rendererDrawColor renderer $= V4 83 50 maxBound maxBound
          SDL.clear renderer

          quit <- handleEvents =<< SDL.pollEvents

          forM_ (components $ renderables comps) $ \r -> do
            SDL.rendererDrawColor renderer $= color r
            SDL.fillRect renderer $ Just $ SDL.Rectangle (P $ V2 (x r) (y r)) (V2 (sideLength r) (sideLength r))

          SDL.present renderer
          let !newRenderables = fmap (\r -> r{x = x r + 1}) (components $ renderables comps)
              !newComps = comps{renderables = (renderables comps){components = newRenderables}}
          newFrameInfo <- calcFpsInfo frameInfo
          unless quit (loop newComps newFrameInfo)
    now <- SDL.ticks
    loop allComponents (now, 0 :: Int)
