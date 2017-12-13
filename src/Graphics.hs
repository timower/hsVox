module Graphics where

import Linear
import Control.Lens
import Control.Monad
import Control.Monad.State
import System.FilePath
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as C3D
import qualified Graphics.GLUtil.JuicyTextures as JT

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IM

import Foreign.Ptr (nullPtr, plusPtr)

import Config

data GfxObj = GfxObj { _vao      :: GL.VertexArrayObject,
                       _program  :: GL.Program,
                       _nVerts   :: GL.NumArrayIndices,
                       _texture  :: GL.TextureObject,
                       _modelMat :: M44 Float,
                       _vbo      :: GL.BufferObject }

makeLenses ''GfxObj

data GfxState = GfxState { _gfxObjs :: IntMap GfxObj,
                           _nextRef :: Int,
                           _camera  :: C3D.Camera Float }

makeLenses ''GfxState

draw :: GfxState -> IO ()
draw state = do
    GL.clearColor $= GL.Color4 1 1 0 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    forM_ (state^.gfxObjs) drawObj
    where
        drawObj obj = do
            GL.currentProgram $= (Just $ obj^.program)
            U.asUniform (getVP state !*! obj^.modelMat) $ GL.UniformLocation 0

            GL.activeTexture $= GL.TextureUnit 0
            GL.textureBinding GL.Texture2D $= (Just $ obj^.texture)

            GL.bindVertexArrayObject $= Just (obj^.vao)
            GL.drawArrays GL.Triangles 0 $ obj^.nVerts
            GL.bindVertexArrayObject $= Nothing


projection :: M44 Float -- TODO: from state/width height?
projection = C3D.projectionMatrix 70.0 (1280 / 720) 0.1 100.0

getVP :: GfxState -> M44 Float
getVP state = projection !*! (C3D.camMatrix $ state^.camera)

-- | add a graphics object to the graphics state
addGfxObj :: GfxObj -> State GfxState Int
addGfxObj obj = do
    objRef <- use nextRef
    nextRef += 1
    gfxObjs %= IM.insert objRef obj
    return objRef

-- | create a graphics state from a list of graphics objects and a camera
gfxFromList :: [GfxObj] -> C3D.Camera Float -> GfxState
gfxFromList objs = GfxState (IM.fromList $ zip [0..] objs) (length objs)

createCube :: GL.Program -> GL.TextureObject -> V3 Float -> IO GfxObj
createCube prog text pos = do
    vbo <- U.makeBuffer GL.ArrayBuffer cubeVerts
    vao <- U.makeVAO $ do
        GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 20 nullPtr)
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
        GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 20 $ plusPtr nullPtr 12)
        GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    return $ GfxObj vao prog 36 text (mkTransformationMat identity pos) vbo

createObj :: GL.Program -> GL.TextureObject -> V3 Float -> Vector Float -> IO GfxObj
createObj prog text pos v = do
    vbo <- U.fromVector GL.ArrayBuffer v
    vao <- U.makeVAO $ do
        GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 20 nullPtr)
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
        GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 20 $ plusPtr nullPtr 12)
        GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    return $ GfxObj vao prog (fromIntegral $ V.length v `div` 5) text (mkTransformationMat identity pos) vbo

updateObj :: GfxState -> Int -> M44 Float -> Vector Float -> IO GfxState
updateObj state ref mmat v = do
    let obj = (state^.gfxObjs) ! ref
    let vobj = obj^.vbo
    GL.bindBuffer GL.ArrayBuffer $= Just vobj
    U.replaceVector GL.ArrayBuffer v
    GL.bindBuffer GL.ArrayBuffer $= Nothing

    return $ gfxObjs.at ref._Just.modelMat .~ mmat $ state

loadTexture :: FilePath -> IO GL.TextureObject
loadTexture path = do
    text <- JT.readTexture $ texturePath </> path

    let Right tobj = text

    GL.textureBinding GL.Texture2D $= Just tobj
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
    GL.textureBinding GL.Texture2D $= Nothing
    return tobj

loadShader :: FilePath -> IO GL.Program
loadShader name = do
    vs <- U.loadShader GL.VertexShader $ shaderPath </> name `addExtension` "vs"
    fs <- U.loadShader GL.FragmentShader $ shaderPath </> name `addExtension` "fs"
    U.linkShaderProgram [vs, fs]

cubeVerts :: [Float]
cubeVerts = [-0.5, -0.5, -0.5,  0.0, 0.0,
            0.5, -0.5, -0.5,  1.0, 0.0,
            0.5,  0.5, -0.5,  1.0, 1.0,
            0.5,  0.5, -0.5,  1.0, 1.0,
            -0.5,  0.5, -0.5,  0.0, 1.0,
            -0.5, -0.5, -0.5,  0.0, 0.0,

            -0.5, -0.5,  0.5,  0.0, 0.0,
            0.5, -0.5,  0.5,  1.0, 0.0,
            0.5,  0.5,  0.5,  1.0, 1.0,
            0.5,  0.5,  0.5,  1.0, 1.0,
            -0.5,  0.5,  0.5,  0.0, 1.0,
            -0.5, -0.5,  0.5,  0.0, 0.0,

            -0.5,  0.5,  0.5,  1.0, 0.0,
            -0.5,  0.5, -0.5,  1.0, 1.0,
            -0.5, -0.5, -0.5,  0.0, 1.0,
            -0.5, -0.5, -0.5,  0.0, 1.0,
            -0.5, -0.5,  0.5,  0.0, 0.0,
            -0.5,  0.5,  0.5,  1.0, 0.0,

            0.5,  0.5,  0.5,  1.0, 0.0,
            0.5,  0.5, -0.5,  1.0, 1.0,
            0.5, -0.5, -0.5,  0.0, 1.0,
            0.5, -0.5, -0.5,  0.0, 1.0,
            0.5, -0.5,  0.5,  0.0, 0.0,
            0.5,  0.5,  0.5,  1.0, 0.0,

            -0.5, -0.5, -0.5,  0.0, 1.0,
            0.5, -0.5, -0.5,  1.0, 1.0,
            0.5, -0.5,  0.5,  1.0, 0.0,
            0.5, -0.5,  0.5,  1.0, 0.0,
            -0.5, -0.5,  0.5,  0.0, 0.0,
            -0.5, -0.5, -0.5,  0.0, 1.0,

            -0.5,  0.5, -0.5,  0.0, 1.0,
            0.5,  0.5, -0.5,  1.0, 1.0,
            0.5,  0.5,  0.5,  1.0, 0.0,
            0.5,  0.5,  0.5,  1.0, 0.0,
            -0.5,  0.5,  0.5,  0.0, 0.0,
            -0.5,  0.5, -0.5,  0.0, 1.0]