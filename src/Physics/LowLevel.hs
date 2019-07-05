{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Physics.LowLevel
  ( Body
  , Shape
  , Constraint
  , Space
  , Physics
  , runPhysics
  , BodyType (..)
  , ShapeType (..)
  , createBody
  , freeBody
  , addBodyToSpace
  , removeBodyFromSpace
  , createShape
  , freeShape
  , addShapeToSpace
  , removeShapeFromSpace

  , bodyPosition
  , shapeElasticity
  , spaceGravity
  )
  where

import qualified Chiphunk.Low as C
import Control.Concurrent
import Data.StateVar
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef
import Polysemy
import Linear.V2
import Control.Monad.IO.Class
import Control.Monad

import Sigma
import StateOperation

data Body = Body {getBody :: C.Body}

data Shape = Shape {getShape :: C.Shape}


data Constraint = Contraint {getConstraint :: C.Constraint}


data Space = Space {getSpace :: C.Space}

data Physics (m :: * -> *) a where
  InstantExecution :: IO a -> Physics m a
  InstantExecutionWithSpace :: (Space -> IO a) -> Physics m a
  SpaceStage :: (Space -> IO ()) -> Physics m ()
  ObjectStage :: IO () -> Physics m ()


makeSem ''Physics

runPhysicsE :: Member (Lift IO) r => Space -> (IO () -> IO ()) -> (IO () -> IO ()) -> Sem (Physics:r) a -> Sem r a
runPhysicsE space executeBefore executeAfter = interpret $ \case
  InstantExecution action -> liftIO action
  InstantExecutionWithSpace action -> liftIO $ action space
  SpaceStage action -> liftIO $ executeBefore (action space)
  ObjectStage action -> liftIO $ executeAfter action



runPhysics :: Member (Lift IO) r => Double -> Signal (Sem (Physics:r)) a b -> Signal (Sem r) a b
runPhysics time physicsSig = Signal $ \a -> do
  space <- Space <$> liftIO C.spaceNew
  executeBeforeRef <- liftIO $ newIORef mempty
  executeAfterRef <- liftIO $ newIORef mempty
  physicsSpaceActions <- liftIO $ newEmptyMVar

  threadId <- liftIO $ forkIO $ forever $ do
    (before, after) <- takeMVar physicsSpaceActions
    before >> after
    C.spaceStep (getSpace space) time


  let addExecuteBefore action = modifyIORef executeBeforeRef (*> action)
      addExecuteAfter action = modifyIORef executeAfterRef (*> action)
      contSignal sig = Signal $ \a -> do
        (b, cont) <- runPhysicsE space addExecuteBefore addExecuteAfter $ stepSignal sig a
        liftIO $ (,) <$> readIORef executeBeforeRef <*> readIORef executeAfterRef >>= putMVar physicsSpaceActions
        liftIO $ writeIORef executeBeforeRef mempty
        liftIO $ writeIORef executeAfterRef mempty

        pure (b, contSignal cont)

  stepSignal (contSignal physicsSig) a


data BodyType = DynamicBody Double Double | KinematicBody | StaticBody

data ShapeType = CircleShape Double (V2 Double) | SegmentShape (V2 Double) (V2 Double) Double | BoxShape Double Double Double

v2ToVect :: V2 Double -> C.Vect
v2ToVect (V2 x y) = C.Vect x y

vectToV2 :: C.Vect -> V2 Double
vectToV2 (C.Vect x y) = V2 x y

v2ToVectStateOperation :: StateOperation (V2 Double) x -> StateOperation (C.Vect) x
v2ToVectStateOperation = mapStateOperation v2ToVect vectToV2

createBody :: Member Physics r => BodyType -> Sem r Body
createBody = fmap Body . instantExecution . \case
  (DynamicBody mass moment) -> C.bodyNew mass moment
  KinematicBody -> C.bodyNewKinematic
  StaticBody -> C.bodyNewStatic

freeBody :: Member Physics r => Body -> Sem r ()
freeBody body = objectStage $ C.bodyFree (getBody body)

createShape :: Member Physics r => Body -> ShapeType -> Sem r Shape
createShape (Body body) = fmap Shape . instantExecution . \case
  (CircleShape radius position) -> C.circleShapeNew body radius (v2ToVect position)
  (SegmentShape positionStart positionEnd thickness) -> C.segmentShapeNew body (v2ToVect positionStart) (v2ToVect positionEnd) thickness
  (BoxShape width height radius) -> C.boxShapeNew body width height radius

freeShape :: Member Physics r => Shape -> Sem r ()
freeShape (Shape shape) = objectStage $ C.shapeFree shape

addShapeToSpace :: Member Physics r => Shape -> Sem r ()
addShapeToSpace (Shape cShape) = spaceStage $ (flip C.spaceAddShape cShape) . getSpace

removeShapeFromSpace :: Member Physics r => Shape -> Sem r ()
removeShapeFromSpace (Shape cShape) = spaceStage $ (flip C.spaceRemoveShape cShape) . getSpace

addBodyToSpace :: Member Physics r => Body -> Sem r ()
addBodyToSpace (Body cBody) = spaceStage $ (flip C.spaceAddBody cBody) . getSpace

removeBodyFromSpace :: Member Physics r => Body -> Sem r ()
removeBodyFromSpace (Body cBody) = spaceStage $ (flip C.spaceRemoveBody cBody) . getSpace


handlePhysicsObjectProperty :: Member Physics r => (obj -> StateVar a) -> obj -> StateOperation a x -> Sem r x
handlePhysicsObjectProperty f obj = \case
  SOSet newValue -> objectStage $ f obj $= newValue
  SOGet mapValue -> fmap mapValue $ instantExecution $ Data.StateVar.get (f obj)

handlePhysicsSpaceProperty :: Member Physics r => (Space -> StateVar a) -> StateOperation a x -> Sem r x
handlePhysicsSpaceProperty f = \case
  SOSet newValue -> spaceStage $ \space -> f space $= newValue
  SOGet mapValue -> fmap mapValue $ instantExecutionWithSpace $ \space -> Data.StateVar.get (f space)


bodyPosition :: Member Physics r => Body -> StateOperation (V2 Double) x -> Sem r x
bodyPosition body stateOperation = handlePhysicsObjectProperty (C.bodyPosition . getBody) body $ v2ToVectStateOperation stateOperation


shapeElasticity :: Member Physics r => Shape -> StateOperation Double x -> Sem r x
shapeElasticity shape stateOperation = handlePhysicsObjectProperty (C.shapeElasticity . getShape) shape stateOperation


spaceGravity :: Member Physics r => StateOperation (V2 Double) x -> Sem r x
spaceGravity = handlePhysicsSpaceProperty (C.spaceGravity . getSpace) . v2ToVectStateOperation
