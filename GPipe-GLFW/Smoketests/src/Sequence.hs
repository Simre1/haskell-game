import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C
import qualified Test.Control as A
import Control.Monad.IO.Class (liftIO)

main = do
    putStrLn "== Multiple sequential windows"
    putStrLn "\tRender a scene on a window and then render again on a second window."
    runContextT GLFW.defaultHandleConfig $ do

        first <- newWindow (WindowFormatColorDepth SRGB8 Depth16) (GLFW.defaultWindowConfig "Sequence 1 (of 2)")
        resources <- C.initRenderContext first [C.yAxis]
        C.mainloop first (A.frames 30) resources C.continue
        deleteWindow first

        second <- newWindow (WindowFormatColorDepth SRGB8 Depth16) (GLFW.defaultWindowConfig "Sequence 2 (of 2)")
        C.mainloop second (A.frames 30) resources C.continue
        deleteWindow second
