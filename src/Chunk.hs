module Chunk where

import qualified Graphics.Rendering.OpenGL as GL
import Linear
import Control.Lens

import Data.Word (Word8)

import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Fusion.Stream.Monadic as S

import Data.Map (Map)
import qualified Data.Map as M

type ChunkPos = V3 Int
-- TODO: import graphics, move stuff from Lib here, rename int to GfxRef
data ChunkState = ChunkState { _chunks :: Map ChunkPos Int}

makeLenses ''ChunkState

type Terrain = Vector Word8

(!!!) :: Terrain -> (Int, Int, Int) -> Word8
terrain !!! (x, y, z) = terrain ! (z * chunkSize * chunkSize + y * chunkSize + x)

fromIdx :: Int -> (Int, Int, Int)
fromIdx i = let z = div i $ chunkSize * chunkSize
                t = i - z * chunkSize * chunkSize
                y = div t chunkSize
                x = t - y * chunkSize
            in (x, y, z)

viconcatMap :: (GV.Vector v a, GV.Vector v b) => (Int -> a -> v b) -> v a -> v b
viconcatMap f = GV.unstream . Bundle.concatVectors . Bundle.inplace (S.map (uncurry f) . S.indexed) id . GV.stream

chunkSize :: Int
chunkSize = 16

loadDist :: Int
loadDist = 1

createTerrain :: V3 Int -> Terrain
createTerrain (V3 cx cy cz) = V.generate (chunkSize*chunkSize*chunkSize) gen
    where
        gen i = let (x', y', z') = fromIdx i
                    x = cx * chunkSize + x'
                    y = cy * chunkSize + y'
                    z = cz * chunkSize + z'
                in  if x*x+y*y+z*z < 8*8
                    then 1
                    else 0


data CubeDir = UpD | DownD | LeftD | RightD | FrontD | BackD

allDirs :: [CubeDir]
allDirs = [UpD, DownD, LeftD, RightD, FrontD, BackD]

addDir :: (Int, Int, Int) -> CubeDir -> (Int, Int, Int)
addDir (x, y, z) UpD    = (x, y+1, z)
addDir (x, y, z) DownD  = (x, y-1, z)
addDir (x, y, z) LeftD  = (x-1, y, z)
addDir (x, y, z) RightD = (x+1, y, z)
addDir (x, y, z) FrontD = (x, y, z-1)
addDir (x, y, z) BackD  = (x, y, z+1)

buildChunk :: Terrain -> Vector Float
buildChunk terrain = viconcatMap buildCube terrain
    where
        buildCube idx d = let pos = fromIdx idx
                            in if d == 0 then V.empty
                                else V.fromList $ concat [buildFace d dir pos |
                                                        dir <- allDirs,
                                                        let dpos@(dx, dy, dz) = addDir pos dir,
                                                        dx < 0 || dx > chunkSize - 1 ||
                                                        dy < 0 || dy > chunkSize - 1 ||
                                                        dz < 0 || dz > chunkSize - 1 ||
                                                        terrain !!! dpos == 0]
        buildFace d dir (x', y', z') =
            let x = fromIntegral x'
                y = fromIntegral y'
                z = fromIntegral z'
            in case dir of
                UpD    -> [ x,   y+1, z,   0.0, 1.0,
                            x+1, y+1, z,   1.0, 1.0,
                            x+1, y+1, z+1, 1.0, 0.0,
                            x+1, y+1, z+1, 1.0, 0.0,
                            x,   y+1, z+1, 0.0, 0.0,
                            x,   y+1, z,   0.0, 1.0]
                DownD  -> [ x,   y, z,     0.0, 1.0,
                            x+1, y, z,     1.0, 1.0,
                            x+1, y, z+1,   1.0, 0.0,
                            x+1, y, z+1,   1.0, 0.0,
                            x,   y, z+1,   0.0, 0.0,
                            x,   y, z,     0.0, 1.0]
                LeftD  -> [ x, y+1, z+1,   1.0, 0.0,
                            x, y+1, z,     1.0, 1.0,
                            x, y,   z,     0.0, 1.0,
                            x, y,   z,     0.0, 1.0,
                            x, y,   z+1,   0.0, 0.0,
                            x, y+1, z+1,   1.0, 0.0]
                RightD -> [ x+1, y+1, z+1, 1.0, 0.0,
                            x+1, y+1, z,   1.0, 1.0,
                            x+1, y,   z,   0.0, 1.0,
                            x+1, y,   z,   0.0, 1.0,
                            x+1, y,   z+1, 0.0, 0.0,
                            x+1, y+1, z+1, 1.0, 0.0]
                FrontD -> [ x,   y,   z,   0.0, 0.0,
                            x+1, y,   z,   1.0, 0.0,
                            x+1, y+1, z,   1.0, 1.0,
                            x+1, y+1, z,   1.0, 1.0,
                            x,   y+1, z,   0.0, 1.0,
                            x,   y,   z,   0.0, 0.0]
                BackD  -> [ x,   y,   z+1, 0.0, 0.0,
                            x+1, y,   z+1, 1.0, 0.0,
                            x+1, y+1, z+1, 1.0, 1.0,
                            x+1, y+1, z+1, 1.0, 1.0,
                            x,   y+1, z+1, 0.0, 1.0,
                            x,   y,   z+1, 0.0, 0.0]
