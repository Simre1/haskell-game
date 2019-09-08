import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (threadDelay)
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Test.Common as C

main :: IO ()
main = do
    putStrLn "== Split thread"
    putStrLn "\tUse shared contexts to load resources on one thread and render on another."
    runContextT GLFW.defaultHandleConfig $ do
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Split")
        -- in main thread, make buffers
        resources <- C.initRenderContext win [C.plane]
        let ((buf:_), _, _) = resources
        -- in other thread change contents of buffers once a second
        withThread (bufferParty buf [C.xAxis, C.yAxis, C.zAxis])
            (C.mainloop win (4 :: Double) resources)
    where
        bufferParty buf [] = liftIO $ print "No more items"
        bufferParty buf (next:items) = do
            liftIO . threadDelay $ round 1e6
            writeBuffer buf 0 next
            bufferParty buf items
