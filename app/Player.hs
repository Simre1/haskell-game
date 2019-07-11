{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Player where

import Control.Arrow (returnA, (>>>), first, (<<<), (***), arr)
import Control.Monad.IO.Class
import Control.Exception (SomeException (SomeException))
import Data.Function ((&))
import Data.Maybe (fromJust)
import Lens.Micro ((.~), (^.), (%~))
import Lens.Micro.TH (makeLenses)
import Polysemy (Member, Sem, Members, Lift)
import Polysemy.Input (Input)
import SDL.Input (keysymKeycode)
import System.IO.Unsafe
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
import Data.Default (Default(..))
import Graphics.ColorSpace
import qualified Data.Massiv.Array as M
import Debug.Trace (traceShow, traceShowId)
import Data.Word (Word8)

import Sigma (Signal, arrM, buildSignal, withInitialization, feedback, stepSignal)
import StateOperation (soSet, soGet)
import Shapes2D (Placed(Placed), Rectangle(Rectangle), Circle(Circle))
import Effect.Input (SDLInput, getKeyState)
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
  , toAnyTexture
  , render
  , Any
  , Texture
  , Static
  , Streaming
  , updateStreamingTexture
  , createStreamingTexture
  )
import Bullets (manageBullets, BulletType (Straight))

data PlayerShip = PlayerShip
  { _playerShipPosition :: V2 Double
  , _playerShipHealth :: Double
  , _playerShipShootCooldown :: Double
  , _playerShipEnergy :: Double
  } deriving Show

instance Default PlayerShip where
  def = PlayerShip (V2 0 0) 0 0 0

makeLenses ''PlayerShip

data PlayerInput = PlayerInput
  { _playerInputDirection :: V2 Int
  , _playerInputShoot :: Bool
  } deriving Show

makeLenses ''PlayerInput

player :: Members [Input SDLInput, (Lift IO), Physics, Graphics] r => Signal (Sem r) () ()
player = feedback def $ proc (_,playerShip) -> do
  playerInput <- collectPlayerInput -< ()
  movedPlayerShip <- movePlayerShip -< (playerShip, playerInput)
  finalPlayerShip <- handleShooting -< (playerInput, movedPlayerShip)
  _ <- renderPlayerShip -< finalPlayerShip
  returnA -< ((), finalPlayerShip)

-- TODO: Write bug report! Up + Left + Arbitrary key do not work when pressed at the same time !!!
collectPlayerInput :: Member (Input SDLInput) r => Signal (Sem r) () PlayerInput
collectPlayerInput = proc _ -> do
  up <- maybe 0 (const 1) <$> getKeyState ((KeycodeW==) . keysymKeycode) -< ()
  down <- maybe 0 (const (-1)) <$> getKeyState ((KeycodeS==) . keysymKeycode) -< ()
  right <- maybe 0 (const 1) <$> getKeyState ((KeycodeD==) . keysymKeycode) -< ()
  left <- maybe 0 (const (-1)) <$> getKeyState ((KeycodeA==) . keysymKeycode) -< ()

  spacebar <- not . null <$> getKeyState ((KeycodeSpace==) . keysymKeycode) -< ()

  returnA -< PlayerInput (V2 (left + right) (up + down)) spacebar

movePlayerShip :: Member Physics r => Signal (Sem r) (PlayerShip, PlayerInput) (PlayerShip)
movePlayerShip = withInitialization createPlayerShipInPhysicsWorld $ flip feedback $
                  arrM $ \((playerShip, playerInput), (playerShipBody, freeAll)) -> do

                    let newPosition = (playerShip ^. playerShipPosition) + calculateMovementVector (fmap fromIntegral (playerInput ^. playerInputDirection))
                    soSet newPosition $ bodyPosition playerShipBody

                    pure (playerShip & playerShipPosition .~ newPosition , (playerShipBody, freeAll))

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

renderPlayerShip :: Member Graphics r => Signal (Sem r) PlayerShip ()
renderPlayerShip = withInitialization ((,) <$> playerShipTexture1 <*> playerShipTexture2) $ \(texture1, texture2) -> feedback 0 $
                     arrM $ \(playerShip, timer) -> do
                       let srcRect = Nothing
                           destRect = pure $ Placed (fmap round (playerShip ^. playerShipPosition)) $ Rectangle 36 72
                           choosenTexture = if timer > 6 then texture1 else texture2
                           newTimer = if timer == 12 then 0 else succ timer
                       render $ makeRenderInstruction 10 choosenTexture srcRect destRect

                       pure ((), newTimer)
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


handleShooting :: (Member Physics r, Member Graphics r) => Signal (Sem r) (PlayerInput, PlayerShip) (PlayerShip)
handleShooting = arr snd <<< (manageBullets 0 *** arr id) <<< (arrM $ \(playerInput, playerShip) -> do
  pure $ if playerShip ^. playerShipShootCooldown <= 0 && playerInput ^. playerInputShoot
    then (pure (Straight, playerShip ^. playerShipPosition + (V2 18 72), Placed (V2 0 0) (Rectangle 800 600)), playerShip & playerShipShootCooldown .~ 0.3)
    else (Nothing, playerShip & playerShipShootCooldown %~ (\x -> x-(1/60))))
