module Input (updateInput) where

import Linear
import GHC.Float
import Control.Lens
import Control.Monad.State
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GLUtil.Camera3D as C3D

import State
import Graphics

getCamVel :: GLFW.Window -> IO (V3 Float)
getCamVel win = do
    f <- GLFW.getKey win GLFW.Key'W
    b <- GLFW.getKey win GLFW.Key'S
    l <- GLFW.getKey win GLFW.Key'A
    r <- GLFW.getKey win GLFW.Key'D
    u <- GLFW.getKey win GLFW.Key'LeftShift
    d <- GLFW.getKey win GLFW.Key'Space
    let conv x = if x == GLFW.KeyState'Released then 0 else 1
    return $ V3 (conv r - conv l) (conv d - conv u) (conv b - conv f)

updateInput :: StateT GameState IO ()
updateInput = do
    (lastX, lastY) <- use $ inputState.mousePos
    win <- use window
    (mouseX, mouseY) <- liftIO $ GLFW.getCursorPos win
    let dx = (lastX - mouseX) * 0.5
    let dy = (lastY - mouseY) * 0.5

    dt <- use dtime
    vel <- liftIO $ getCamVel win
    gfxState.camera %= C3D.dolly (5 * dt *^ vel) . C3D.panGlobal (double2Float dx) . C3D.tilt (double2Float dy)
    inputState.mousePos .= (mouseX, mouseY)
    return ()