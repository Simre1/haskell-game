module Graphics.Shader.ViewPort (adjustViewPort) where

import Linear.V2 (V2(..))

import Window.GPipe (ViewPort(..))

adjustViewPort :: V2 Int -> ViewPort -> ViewPort
adjustViewPort (V2 winX winY) (ViewPort (V2 posX posY) (V2 logicalSizeX logicalSizeY)) =
    let sizeY = winY * logicalSizeY `quot` 480
        sizeX = winY * logicalSizeX `quot` 480
        newPosY = winY * posY `quot` 480
        newPosX = ((winX - winY) `quot` 2) + winY * posX `quot` 480
    in (ViewPort (V2 newPosX newPosY) (V2 sizeX sizeY))
