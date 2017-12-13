module Config where

import System.FilePath

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