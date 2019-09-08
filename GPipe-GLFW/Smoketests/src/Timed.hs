import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C
import qualified Test.Control as A

main :: IO ()
main = do
    putStrLn "== Timed render"
    putStrLn $ "\tRender a scene to a window for " ++ show maxSec ++ " seconds."
    runContextT GLFW.defaultHandleConfig $ do
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Timed")
        resources <- C.initRenderContext win [C.xAxis, C.yAxis, C.zAxis, C.plane]
        C.mainloop win (A.seconds maxSec) resources C.continue
    where
        maxSec = 2.0
