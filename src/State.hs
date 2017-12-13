module State where

import Linear
import Control.Lens
import Data.Map (Map)
import qualified Graphics.UI.GLFW as GLFW

import Graphics

newtype InputState = InputState { _mousePos :: (Double, Double) }

makeLenses ''InputState

type ChunkPos = V3 Int
-- TODO: import graphics, move stuff from Lib here, rename int to GfxRef
data ChunkState = ChunkState { _chunks :: Map ChunkPos Int}

makeLenses ''ChunkState

data GameState = GameState { _time          :: Float,
                             _dtime         :: Float,
                             _window        :: GLFW.Window,
                             _gfxState      :: GfxState,
                             _inputState    :: InputState,
                             _chunkState    :: ChunkState }

makeLenses ''GameState