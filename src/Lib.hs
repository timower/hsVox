module Lib
    ( initialize,
      mainLoop,
      cleanup
    ) where

import Linear
import GHC.Float
import System.IO
import System.Exit
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromJust)
import System.FilePath ((</>))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))

import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as C3D

import Control.Lens

import State
import Chunk
import Input
import Config
import Graphics

import qualified Data.Map as M

errorCallback :: GLFW.ErrorCallback
errorCallback e s = hPutStrLn stderr $ (show e) ++ ": " ++ s

initialize :: IO GameState
initialize = do
    win <- initWindow
    initGame win

initWindow :: IO GLFW.Window
initWindow = do
    GLFW.setErrorCallback (Just errorCallback)
    initSuccess <- GLFW.init
    if not initSuccess then exitFailure else do
         GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
         GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
         GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
         GLFW.windowHint $ GLFW.WindowHint'Resizable False
         mw <- GLFW.createWindow width height title Nothing Nothing
         case mw of
             Nothing -> GLFW.terminate >> exitFailure
             Just win -> do
                 GLFW.makeContextCurrent mw
                 GLFW.setKeyCallback win Nothing
                 GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
                 GLFW.swapInterval 1
                 return win

initGame :: GLFW.Window -> IO GameState
initGame win = do
    GL.cullFace $= Nothing -- TODO: fix face culling in chunk & cube
    GL.depthFunc $= Just GL.Less

    prog <- loadShader "simple"
    tobj <- loadTexture "wall.jpg"

    cube1 <- createCube prog tobj $ V3 0 10 0
    cube2 <- createCube prog tobj $ V3 2 10 0
    cube3 <- createCube prog tobj $ V3 0 12 0
    cube4 <- createCube prog tobj $ V3 0 10 2
    let cubes = [cube1, cube2, cube3, cube4]

    let camera = C3D.dolly (V3 3 10 3) C3D.fpsCamera
    let gfx1 = gfxFromList cubes camera

    (chunkS, gfx2) <- runStateT initChunks gfx1

    mousePos <- GLFW.getCursorPos win
    return $ GameState 0 0 win gfx2 (InputState mousePos) chunkS

cleanup :: GameState -> IO ()
cleanup state = do
    GLFW.destroyWindow $ state^.window
    GLFW.terminate
    return ()

mainLoop :: GameState -> IO ()
mainLoop state = do
    let win = state^.window
    close <- GLFW.windowShouldClose win
    unless close $ do
        state' <- execStateT updateState state
        draw $ state' ^. gfxState

        GLFW.swapBuffers win
        GLFW.pollEvents
        let dt = state' ^. dtime
        GLFW.setWindowTitle win $ "fps: " ++ (show $ truncate $ 1/dt) ++ " ms: " ++ (show $ truncate $ dt*1000) -- ++ " pos: " ++ (show $ fmap floor $ C3D.location $ getCamera $ getGfxState state)
        mainLoop state'

updateState :: StateT GameState IO ()
updateState = do
    t <- liftIO getTime
    told <- use time
    dtime .= t - told
    time .= t

    updateInput
    updateChunks
  where
      getTime = GLFW.getTime >>= (return . double2Float . fromJust)