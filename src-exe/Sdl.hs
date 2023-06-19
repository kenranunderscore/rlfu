{-# LANGUAGE OverloadedStrings #-}

module Sdl where

import Control.Exception qualified as E
import SDL qualified
import SDL.Vect

withSdlEnvironment :: (SDL.Window -> SDL.Renderer -> IO a) -> IO a
withSdlEnvironment f =
  withSdl $
    withWindow $ \win ->
      withRenderer win $ \renderer -> f win renderer

withRenderer :: SDL.Window -> (SDL.Renderer -> IO a) -> IO a
withRenderer win =
  E.bracket
    ( do
        putStrLn "Creating SDL renderer"
        SDL.createRenderer
          win
          (-1)
          SDL.RendererConfig
            { SDL.rendererType = SDL.AcceleratedRenderer,
              SDL.rendererTargetTexture = False
            }
    )
    ( \renderer -> do
        putStrLn "Destroying renderer"
        SDL.destroyRenderer renderer
    )

withWindow :: (SDL.Window -> IO a) -> IO a
withWindow =
  E.bracket
    ( do
        putStrLn "Creating SDL window"
        win <- SDL.createWindow "foo" SDL.defaultWindow {SDL.windowInitialSize = V2 800 600}
        SDL.showWindow win
        pure win
    )
    ( \win -> do
        putStrLn "Destroying window"
        SDL.destroyWindow win
    )

withSdl :: IO a -> IO a
withSdl =
  E.bracket_
    ( do
        putStrLn "Initializing SDL"
        SDL.initialize [SDL.InitVideo]
    )
    ( do
        putStrLn "Quitting SDL"
        SDL.quit
    )
