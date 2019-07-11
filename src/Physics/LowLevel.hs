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
  , ConstraintType (..)
  , PinJoint
  , createBody
  , freeBody
  , addBodyToSpace
  , removeBodyFromSpace
  , createShape
  , freeShape
  , addShapeToSpace
  , removeShapeFromSpace
  , createConstraint
  , freeConstraint
  , addConstraintToSpace
  , removeConstraintFromSpace
  , bodyPosition
  , shapeElasticity
  , spaceGravity
  , shapeFilter
  , bodyVelocity
  , shapeSensor
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
import Shapes2D



data Body = Body {getBody :: C.Body}

data Shape = Shape {getShape :: C.Shape}


data Constraint constraintType = Constraint {getConstraint :: C.Constraint}

data Space = Space {getSpace :: C.Space}

data Physics (m :: * -> *) a where
  ExecutePhysics :: IO a -> Physics m a
  ExecutePhysicsWithSpace :: (Space -> IO a) -> Physics m a


data ShapeFilter = ShapeFilter
makeSem ''Physics

runPhysicsE :: Member (Lift IO) r => Space -> Sem (Physics:r) a -> Sem r a
runPhysicsE space = interpret $ \case
  ExecutePhysics action -> liftIO action
  ExecutePhysicsWithSpace action -> liftIO $ action space


runPhysics :: Member (Lift IO) r => Double -> Signal (Sem (Physics:r)) a b -> Signal (Sem r) a b
runPhysics time physicsSig = Signal $ \a -> do
  space <- Space <$> liftIO C.spaceNew
  wait <- liftIO $ newEmptyMVar
  threadId <- liftIO $ forkIO $ forever $ do
    takeMVar wait
    C.spaceStep (getSpace space) time


  let contSignal sig = Signal $ \a -> do
        (b, cont) <- runPhysicsE space $ stepSignal sig a
        liftIO $ tryTakeMVar wait >> putMVar wait ()
        pure (b, contSignal cont)

  x <- stepSignal (contSignal physicsSig) a
  liftIO $ putMVar wait ()
  pure x


data BodyType = DynamicBody Double Double | KinematicBody | StaticBody

data ShapeType = CircleShape (Circle Double) (V2 Double) | SegmentShape (Line Double) Double | BoxShape (Rectangle Double) Double

v2ToVect :: V2 Double -> C.Vect
v2ToVect (V2 x y) = C.Vect x y

vectToV2 :: C.Vect -> V2 Double
vectToV2 (C.Vect x y) = V2 x y

v2ToVectStateOperation :: StateOperation (V2 Double) x -> StateOperation (C.Vect) x
v2ToVectStateOperation = mapStateOperation v2ToVect vectToV2

createBody :: Member Physics r => BodyType -> Sem r Body
createBody = fmap Body . executePhysics . \case
  (DynamicBody mass moment) -> C.bodyNew mass moment
  KinematicBody -> C.bodyNewKinematic
  StaticBody -> C.bodyNewStatic

freeBody :: Member Physics r => Body -> Sem r ()
freeBody body = executePhysics $ C.bodyFree (getBody body)

createShape :: Member Physics r => Body -> ShapeType -> Sem r Shape
createShape (Body body) = fmap Shape . executePhysics . \case
  (CircleShape (Circle radius) position) -> C.circleShapeNew body radius (v2ToVect position)
  (SegmentShape (Line positionStart positionEnd) thickness) -> C.segmentShapeNew body (v2ToVect positionStart) (v2ToVect positionEnd) thickness
  (BoxShape (Rectangle width height) radius) -> C.boxShapeNew body width height radius

freeShape :: Member Physics r => Shape -> Sem r ()
freeShape (Shape shape) = executePhysics $ C.shapeFree shape

addShapeToSpace :: Member Physics r => Shape -> Sem r ()
addShapeToSpace (Shape cShape) = executePhysicsWithSpace $ (flip C.spaceAddShape cShape) . getSpace

removeShapeFromSpace :: Member Physics r => Shape -> Sem r ()
removeShapeFromSpace (Shape cShape) = executePhysicsWithSpace $ (flip C.spaceRemoveShape cShape) . getSpace

addBodyToSpace :: Member Physics r => Body -> Sem r ()
addBodyToSpace (Body cBody) = executePhysicsWithSpace $ (flip C.spaceAddBody cBody) . getSpace

removeBodyFromSpace :: Member Physics r => Body -> Sem r ()
removeBodyFromSpace (Body cBody) = executePhysicsWithSpace $ (flip C.spaceRemoveBody cBody) . getSpace

data PinJoint

data ConstraintType constraintType where
  PinJoint :: (Body, V2 Double) -> (Body, V2 Double) -> ConstraintType PinJoint


createConstraint :: Member Physics r => ConstraintType constraintType -> Sem r (Constraint constraintType)
createConstraint = fmap Constraint . executePhysics . \case
  PinJoint (Body cBody1, anchor1) (Body cBody2, anchor2) -> C.pinJointNew cBody1 cBody2 (v2ToVect anchor1) (v2ToVect anchor2)

freeConstraint :: Member Physics r => Constraint a -> Sem r ()
freeConstraint (Constraint cConstraint) = executePhysics $ C.constraintFree cConstraint

addConstraintToSpace :: Member Physics r => Constraint a -> Sem r ()
addConstraintToSpace (Constraint cConstraint) = executePhysicsWithSpace $ flip C.spaceAddConstraint cConstraint . getSpace

removeConstraintFromSpace :: Member Physics r => Constraint a -> Sem r ()
removeConstraintFromSpace (Constraint cConstraint) = executePhysicsWithSpace $ (flip C.spaceRemoveConstraint cConstraint) . getSpace


handlePhysicsObjectProperty :: Member Physics r => (obj -> StateVar a) -> obj -> StateOperation a x -> Sem r x
handlePhysicsObjectProperty f obj = \case
  SOSet newValue -> executePhysics $ f obj $= newValue
  SOGet mapValue -> fmap mapValue $ executePhysics $ Data.StateVar.get (f obj)

handlePhysicsSpaceProperty :: Member Physics r => (Space -> StateVar a) -> StateOperation a x -> Sem r x
handlePhysicsSpaceProperty f = \case
  SOSet newValue -> executePhysicsWithSpace $ \space -> f space $= newValue
  SOGet mapValue -> fmap mapValue $ executePhysicsWithSpace $ \space -> Data.StateVar.get (f space)


bodyPosition :: Member Physics r => Body -> StateOperation (V2 Double) x -> Sem r x
bodyPosition body stateOperation = handlePhysicsObjectProperty (C.bodyPosition . getBody) body $ v2ToVectStateOperation stateOperation

bodyVelocity :: Member Physics r => Body -> StateOperation (V2 Double) x -> Sem r x
bodyVelocity body stateOperation = handlePhysicsObjectProperty (C.bodyVelocity . getBody) body $ v2ToVectStateOperation stateOperation

shapeElasticity :: Member Physics r => Shape -> StateOperation Double x -> Sem r x
shapeElasticity shape stateOperation = handlePhysicsObjectProperty (C.shapeElasticity . getShape) shape stateOperation

shapeFilter :: Member Physics r => Shape -> StateOperation C.ShapeFilter x -> Sem r x
shapeFilter shape stateOperation = handlePhysicsObjectProperty (C.shapeFilter . getShape) shape stateOperation

shapeSensor :: Member Physics r => Shape -> StateOperation Bool x -> Sem r x
shapeSensor shape stateOperation = handlePhysicsObjectProperty (C.shapeSensor . getShape) shape stateOperation


spaceGravity :: Member Physics r => StateOperation (V2 Double) x -> Sem r x
spaceGravity = handlePhysicsSpaceProperty (C.spaceGravity . getSpace) . v2ToVectStateOperation
