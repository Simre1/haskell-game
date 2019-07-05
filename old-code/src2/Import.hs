module Import
  (module X) where

import CorePrelude as X hiding ((.), first, second)
import Control.Category as X
import Control.Arrow as X


import Polysemy as X
import Polysemy.Reader as X
import Polysemy.State as X


import Control.Applicative as X
import Control.Monad as X
import Data.Default as X
import Linear.V2 as X
import Data.Colour as X
import Data.Colour.SRGB as X
--import Data.StateVar as X
import Data.Colour.Names as X hiding (tan)
import Data.Semigroup as X
import Data.Functor.Identity as X
