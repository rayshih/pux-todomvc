module App.Events where

import Control.Monad.Eff.Console (CONSOLE)
import Network.HTTP.Affjax (AJAX)

type AppEffects fx = (ajax :: AJAX, console :: CONSOLE | fx)
