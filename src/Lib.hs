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
import qualified Data.Vector.Storable as V
import Data.List ((\\))

import Control.Lens

import Chunk
import Graphics

import Data.Map ((!))
import qualified Data.Map as M

newtype InputState = InputState { _mousePos :: (Double, Double) }

makeLenses ''InputState

data GameState = GameState { _time          :: Float,
                             _dtime         :: Float,
                             _window        :: GLFW.Window,
                             _gfxState      :: GfxState,
                             _inputState    :: InputState,
                             _chunkState    :: ChunkState }

makeLenses ''GameState

errorCallback :: GLFW.ErrorCallback
errorCallback e s = hPutStrLn stderr $ (show e) ++ ": " ++ s

initialize :: IO GameState
initialize = do
    win <- initWindow
    initResources win

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
             Just window -> do
                 GLFW.makeContextCurrent mw
                 GLFW.setKeyCallback window Nothing
                 GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
                 GLFW.swapInterval 1
                 return window

initResources :: GLFW.Window -> IO GameState
initResources win = do
    GL.cullFace $= Nothing -- TODO: fix face culling in chunk & cube
    GL.depthFunc $= Just GL.Less
    vs <- U.loadShader GL.VertexShader $ shaderPath </> "simple.vs"
    fs <- U.loadShader GL.FragmentShader $ shaderPath </> "simple.fs"

    prog <- U.linkShaderProgram [vs, fs]

    tobj <- loadTexture $ texturePath </> "wall.jpg"

    cube1 <- createCube prog tobj $ V3 0 10 0
    cube2 <- createCube prog tobj $ V3 2 10 0
    cube3 <- createCube prog tobj $ V3 0 12 0
    cube4 <- createCube prog tobj $ V3 0 10 2
    let cubes = [cube1, cube2, cube3, cube4]
    let camera = C3D.dolly (V3 3 10 3) C3D.fpsCamera
    let gfx1 = gfxFromList cubes camera

    let cps = [V3 x y z | x <- [-loadDist..loadDist], y <- [-loadDist..loadDist], z <- [-loadDist..loadDist]]
    tobj2 <- loadTexture $ texturePath </> "crate.png"
    cGfxObjs <- mapM (loadChunk prog tobj2) cps
    let (refs, gfx2) = runState (mapM addGfxObj cGfxObjs) gfx1

    mousePos <- GLFW.getCursorPos win
    t <- getTime
    return $ GameState t 0 win gfx2 (InputState mousePos) (ChunkState $ M.fromList $ zip cps refs)

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

-- TODO: make StateT
updateState :: StateT GameState IO ()
updateState = do
    t <- liftIO getTime
    told <- use time
    dtime .= t - told
    time .= t

    updateInput
    updateChunks

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

updateChunks :: StateT GameState IO ()
updateChunks = do
    cam <- use $ gfxState.camera
    let pp@(V3 px py pz) = fmap (flip div chunkSize . floor) $ C3D.location cam
    let shouldBeLoaded = [V3 x y z | x <- [px-loadDist..px+loadDist], y <- [py-loadDist..py+loadDist], z <- [pz-loadDist..pz+loadDist]]
    -- make list of gfxRefs from chunks that are too far
    cs <- use $ chunkState.chunks
    let loaded = M.keys cs
    let toofar = loaded \\ shouldBeLoaded
    let gfxRefs = (map (cs !) toofar) :: [Int]
    let cs' = foldr M.delete cs toofar
    -- update chunks that should be loaded with these gfxRefs
    let toload = shouldBeLoaded \\ loaded
    gstate <- use gfxState
    (cs'', gfxState') <- liftIO $ foldM updateCs (cs', gstate) $ zip toload gfxRefs
    chunkState.chunks .= cs''
    gfxState .= gfxState'
    return ()
  where
      updateCs :: (M.Map ChunkPos Int, GfxState) -> (ChunkPos, Int) -> IO (M.Map ChunkPos Int, GfxState)
      updateCs (map, gfxState) (p, r) = do
        let drawPos = fmap (fromIntegral . (*chunkSize)) p
        let mmat = mkTransformationMat identity drawPos
        gfxState' <- updateObj gfxState r mmat $ buildChunk $ createTerrain p
        return (M.insert p r map, gfxState')
    -- chunkPos <- use $ chunkState.pos
    -- unless (chunkPos == playerPos) $ do
    --     let chunk = buildChunk $ createTerrain playerPos
    --     vbo <- use $ chunkState.vbo
    --     liftIO $ do
    --         GL.bindBuffer GL.ArrayBuffer $= Just vbo
    --         U.replaceVector GL.ArrayBuffer chunk
    --         GL.bindBuffer GL.ArrayBuffer $= Nothing
    --     chunkState.pos .= playerPos
    --     let drawPos = fmap (fromIntegral . (*chunkSize)) playerPos
    --     gfxState.gfxObjs.(at 0)._Just.modelMat .= mkTransformationMat identity drawPos
    --     gfxState.gfxObjs.(at 0)._Just.nVerts .= (fromIntegral $ V.length chunk `div` 5)
    --     return ()

getTime :: IO Float
getTime = GLFW.getTime >>= (return . double2Float . fromJust)

-- | loads a chunk and returns the gfx object and gfx ref, for init
loadChunk :: GL.Program -> GL.TextureObject -> V3 Int -> IO GfxObj
loadChunk p t cpos = createObj p t (fmap (fromIntegral . (*chunkSize)) cpos) $ buildChunk $ createTerrain cpos

shaderPath :: FilePath
shaderPath = "res" </> "shaders"

texturePath :: FilePath
texturePath = "res" </> "textures"

title :: String
title = "hsVox - test"

width :: Int
width = 1280

height :: Int
height = 720