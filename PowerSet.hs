module PowerSet where

import Control.Monad

powerSet = filterM (const [True,False])

--explanation for this at https://abhiroop.github.io/Haskell-Powerset/
