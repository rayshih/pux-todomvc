module App.Events where

import Data.Function (($))
import Network.HTTP.Affjax (AJAX)

type AppEffects fx = (ajax :: AJAX | fx)
