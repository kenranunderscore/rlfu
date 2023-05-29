{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Function ((&))
import SDL (($=))
import SDL qualified
import SDL.Vect

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  win <- SDL.createWindow "foo" SDL.defaultWindow {SDL.windowInitialSize = V2 640 480}
  SDL.showWindow win
  renderer <-
    SDL.createRenderer
      win
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer,
          SDL.rendererTargetTexture = False
        }
  SDL.rendererDrawColor renderer $= V4 83 50 maxBound maxBound
  let loop = do
        evts <- fmap SDL.eventPayload <$> SDL.pollEvents
        let quit =
              evts
                & fmap
                  ( \case
                      SDL.KeyboardEvent e | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                        case SDL.keysymKeycode (SDL.keyboardEventKeysym e) of
                          SDL.KeycodeEscape -> True
                          _ -> False
                      SDL.QuitEvent -> True
                      _ -> False
                  )
                & or
        SDL.clear renderer
        SDL.present renderer
        unless quit loop
  loop
  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit
