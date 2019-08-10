module Aux.Polysemy.Input
  (runInputWithSignal) where


import Polysemy
import Polysemy.Input
import Polysemy.Reader
import Sigma.Signal

runInputWithSignal :: Signal (Sem r) i -> Signal (Sem (Input i : r)) b -> Signal (Sem r) b
runInputWithSignal calcNewI signal = readerSignal calcNewI $ signalMorph reinterpretInput signal
  where reinterpretInput :: Sem (Input i : r) a -> Sem (Reader i : r) a
        reinterpretInput = reinterpret (\Input -> ask)
