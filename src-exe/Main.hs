{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foreign.C.Types (CInt)
import ECS
import Data.Word (Word8)
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

main :: IO ()
main = do
  let e = fst $ createEntity allEntities
      renderables = addComponent e (Renderable 100 200 (V4 100 maxBound 20 maxBound) 30) emptyComponentArray
  print renderables
  Sdl.withSdlEnvironment $ \_win renderer -> do
    let loop = do
          SDL.rendererDrawColor renderer $= V4 83 50 maxBound maxBound
          SDL.clear renderer

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

          forM_ (components renderables) $ \r -> do
            SDL.rendererDrawColor renderer $= color r
            SDL.fillRect renderer $ Just $ SDL.Rectangle (P $ V2 (x r) (y r)) (V2 (sideLength r) (sideLength r))

          SDL.present renderer
          unless quit loop
    loop
