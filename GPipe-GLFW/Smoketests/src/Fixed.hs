import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C
import qualified Test.Control as A

main :: IO ()
main = do
    putStrLn "== Fixed frame count"
    putStrLn $ "\tRender a scene to a window for " ++ show maxFrames ++ " frames."
    runContextT GLFW.defaultHandleConfig $ do
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Fixed")
        resources <- C.initRenderContext win [C.xAxis, C.yAxis, C.zAxis, C.plane]
        C.mainloop win (A.frames maxFrames) resources C.continue
    where
        maxFrames = 60
