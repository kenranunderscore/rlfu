{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Function ((&))
import SDL (($=))
import SDL qualified
import SDL.Vect
import Sdl qualified

main :: IO ()
main =
  Sdl.withSdlEnvironment $ \_win renderer -> do
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
