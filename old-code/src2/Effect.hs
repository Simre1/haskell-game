module Effect (runLiftedEffect) where

import Import
import Sigma

runLiftedEffect :: Functor m => Signal (LiftC m) a b -> Signal m a b
runLiftedEffect = signalSimpleMorph runM
