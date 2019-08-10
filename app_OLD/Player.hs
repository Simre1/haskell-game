{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Player where


import Control.Monad.IO.Class
import Control.Exception (SomeException (SomeException))
import Data.Function ((&))
import Data.Maybe (fromJust)
import Lens.Micro ((.~), (^.), (%~))
import Lens.Micro.TH (makeLenses)
import Polysemy (Member, Sem, Members, Lift)
import Polysemy.Reader (Reader, ask)
import Polysemy.State (State, get, put)
import Polysemy.Input (Input)
import SDL.Input (keysymKeycode)
import SDL.Input.Keyboard.Codes
  ( pattern KeycodeUp
  , pattern KeycodeDown
  , pattern KeycodeLeft
  , pattern KeycodeRight
  , pattern KeycodeSpace
  , pattern KeycodeW
  , pattern KeycodeA
  , pattern KeycodeS
  , pattern KeycodeD
  )
import Data.Either (fromRight, fromLeft, either)
import Linear.V2 (V2(..))
import Data.Maybe (maybe)
import qualified Data.Massiv.Array as M
import Data.Word (Word8)

import Sigma.Signal (Signal, buildSignal, withInitialization, feedback, stepSignal, readerSignal, stateSignal, liftSem)
import StateOperation (soSet, soGet)
import Shapes2D (Placed(Placed), Rectangle(Rectangle), Circle(Circle))
import Effect.Input (SDLInput, getKeyState)
import Effect.GlobalState (GlobalState)
import Effect.Physics
  ( createBody
  , addBodyToSpace
  , removeBodyFromSpace
  , freeBody
  , createShape
  , addShapeToSpace
  , removeShapeFromSpace
  , freeShape
  , bodyPosition
  , bodyVelocity
  , shapeSensor
  , BodyType (KinematicBody, DynamicBody)
  , ShapeType (CircleShape)
  , Physics
  , Body
  )
import Effect.Graphics
  ( Graphics
  , Image
  , loadImage
  , makeTexture
  , createStaticTexture
  , updateStaticTexture
  , makeRenderInstruction
  , defaultRenderInstruction
  , riZIndex
  , riTextureArea
  , riScreenArea
  , toAnyTexture
  , render
  , Any
  , Texture
  , Static
  , Streaming
  , updateStreamingTexture
  , createStreamingTexture
  )
import Bullets (signalSpawnBullet, BulletType (Straight), Bullets)

data PlayerShip = PlayerShip
  { _playerShipPosition :: V2 Double
  , _playerShipHealth :: Double
  , _playerShipShootCooldown :: Double
  , _playerShipEnergy :: Double
  } deriving Show

makeLenses ''PlayerShip

data PlayerInput = PlayerInput
  { _playerInputDirection :: V2 Int
  , _playerInputShoot :: Bool
  } deriving Show

makeLenses ''PlayerInput
--
player :: Members [Input SDLInput, (Lift IO), Physics, Graphics, GlobalState Bullets] r => Signal r ()
player = feedback (PlayerShip (V2 0 0) 100 0 0) $
  readerSignal collectPlayerInput $
    movePlayerShip *> handleShooting *> renderPlayerShip

-- TODO: Write bug report! Up + Left + Arbitrary key do not work when pressed at the same time !!!
collectPlayerInput :: Member (Input SDLInput) r => Signal r PlayerInput
collectPlayerInput =
  makePlayerInput
    <$> (maybe 0 (const 1) <$> getKeyState ((KeycodeW==) . keysymKeycode))
    <*> (maybe 0 (const (-1)) <$> getKeyState ((KeycodeS==) . keysymKeycode))
    <*> (maybe 0 (const 1) <$> getKeyState ((KeycodeD==) . keysymKeycode))
    <*> (maybe 0 (const (-1)) <$> getKeyState ((KeycodeA==) . keysymKeycode))
    <*> (not . null <$> getKeyState ((KeycodeSpace==) . keysymKeycode))
    where makePlayerInput up down right left spacebar = PlayerInput (V2 (left + right) (up + down)) spacebar

movePlayerShip :: (Member Physics r, Member (Reader PlayerInput) r, Member (State PlayerShip) r) => Signal r ()
movePlayerShip = withInitialization createPlayerShipInPhysicsWorld $ \(playerShipBody, freeAll) -> liftSem $ do
                  playerInput <- ask
                  playerShip <- get
                  let newPosition = (playerShip ^. playerShipPosition) + calculateMovementVector (fmap fromIntegral (playerInput ^. playerInputDirection))
                  soSet newPosition $ bodyPosition playerShipBody
                  put $ playerShip & playerShipPosition .~ newPosition
                  pure ()
  where
    calculateMovementVector = (*2) . \case
            (V2 1 1) -> V2 (sqrt 0.5) (sqrt 0.5)
            (V2 (-1) (-1)) -> negate <$> V2 (sqrt 0.5) (sqrt 0.5)
            (V2 x y) -> V2 x y

    createPlayerShipInPhysicsWorld :: Member Physics r => Sem r (Body, Sem r ())
    createPlayerShipInPhysicsWorld = do
          spaceShipBody <- createBody KinematicBody
          spaceShipShape <- createShape spaceShipBody $ CircleShape (Circle 30) (V2 0 0)
          spaceShipShape2 <- createShape spaceShipBody $ CircleShape (Circle 30) (V2 0 0)

          addBodyToSpace spaceShipBody
          addShapeToSpace spaceShipShape

          pure ( spaceShipBody, do
                   freeShape spaceShipShape
                   freeBody spaceShipBody
                   removeShapeFromSpace spaceShipShape
                   removeBodyFromSpace spaceShipBody
                )

renderPlayerShip :: (Member Graphics r, Member (State PlayerShip) r) => Signal r ()
renderPlayerShip = withInitialization ((,) <$> playerShipTexture1 <*> playerShipTexture2) $ \(texture1, texture2) -> feedback (0 :: Int) $
                     liftSem $ do
                       playerShip <- get
                       timer <- get @Int
                       let srcRect = Nothing
                           destRect = pure $ Placed (fmap round (playerShip ^. playerShipPosition)) $ Rectangle 36 72
                           choosenTexture = if timer > 6 then texture1 else texture2
                           newTimer = if timer == 12 then 0 else succ timer
                       render $ defaultRenderInstruction choosenTexture &
                                  riZIndex .~ 10 &
                                  riTextureArea .~ srcRect &
                                  riScreenArea .~ destRect
                       put newTimer
                       pure ()
  where
    playerShipTexture1 :: Member Graphics r => Sem r (Texture Static)
    playerShipTexture1 = makeTexture $ do
      texture <- createStaticTexture $ Rectangle 36 72
      imgEither <- loadImage "./media/ship1.bmp"
      let img = fromRight defaultImage imgEither
      --either (\a -> traceShow a $ pure ()) (const $ pure ()) imgEither
      updateStaticTexture texture Nothing img
      pure texture
    playerShipTexture2 :: Member Graphics r => Sem r (Texture Static)
    playerShipTexture2 = makeTexture $ do
      texture <- createStaticTexture $ Rectangle 36 72
      imgEither <- loadImage "./media/ship2.bmp"
      let img = fromRight defaultImage imgEither


      updateStaticTexture texture Nothing img
      pure texture

      pure texture
    defaultImage :: Image
    defaultImage = M.makeArray M.Seq (M.Sz2 36 72) $ \_ -> (50,200,200,100)


handleShooting :: (Members [Lift IO, GlobalState Bullets, State PlayerShip, Reader PlayerInput] r) => Signal r ()
handleShooting =
  flip readerSignal signalSpawnBullet $ liftSem $ do
    playerShip <- get
    playerInput <- ask
    shouldSpawnBullet <- if playerShip ^. playerShipShootCooldown <= 0 && playerInput ^. playerInputShoot
      then put (playerShip & playerShipShootCooldown .~ 0.3) *> pure (pure (Straight, playerShip ^. playerShipPosition + (V2 18 72)))
      else put (playerShip & playerShipShootCooldown %~ (\x -> x-(1/60))) *> pure Nothing
    pure shouldSpawnBullet
