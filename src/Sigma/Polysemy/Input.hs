{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Sigma.Polysemy.Input
  (runInputWithSignal) where


import Polysemy (reinterpret, Sem)
import Polysemy.Input (Input, Input(Input))
import Polysemy.Reader (ask, Reader)

import Sigma.Signal (readerSignal, signalMorph, Signal, stepSignal)


runInputWithSignal :: Signal (Sem r) i -> Signal (Sem (Input i : r)) b -> Signal (Sem r) b
runInputWithSignal calcNewI signal = readerSignal calcNewI $ signalMorph reinterpretInput signal
  where reinterpretInput :: Sem (Input i : r) a -> Sem (Reader i : r) a
        reinterpretInput = reinterpret (\Input -> ask)
