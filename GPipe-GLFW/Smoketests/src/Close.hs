import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C
import qualified Test.Control as A
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

main = do
    putStrLn "== Window-should-close interfaces"
    putStrLn $ "\tRender a scene using additional additional GLFW interfaces."
    runContextT handleConfig $ do
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Window-should-close")
        GLFW.setWindowCloseCallback win $ Just onCloseButton
        resources <- C.initRenderContext win [C.xAxis, C.yAxis, C.zAxis]
        C.mainloop win (A.repeat $ A.seconds 1.0) resources $ \controller -> do
            Just t <- liftIO $ GLFW.getTime
            when (t > 3.5) $ do
                liftIO $ putStrLn "!! Programmatically setting window-close bit"
                Just () <- GLFW.setWindowShouldClose win True
                return ()
            shouldClose <- GLFW.windowShouldClose win
            return $ maybe False id shouldClose
    where
        onCloseButton = putStrLn "!! Window-close button pressed"
        handleConfig = GLFW.defaultHandleConfig {GLFW.configErrorCallback=curry print :: GLFW.Error -> String -> IO ()}
