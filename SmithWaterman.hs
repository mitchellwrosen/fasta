module SmithWaterman where

import Control.Monad.State

smithWaterman :: String -> String -> Int
smithWaterman s1 s2 = execState (smithWaterman'
