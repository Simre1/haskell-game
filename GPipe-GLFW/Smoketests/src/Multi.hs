import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

main :: IO ()
main = do
    putStrLn "== Multi window test"
    putStrLn "\tUse shared contexts to load resources and render different subsets to different windows."
    runContextT GLFW.defaultHandleConfig $ do
        win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Multi")
        -- TODO: in this thread render the axes
        -- TODO: fork; in other thread render just C.plane
        return ()
