module App.Events where

import Network.HTTP.Affjax (AJAX)

type AppEffects fx = (ajax :: AJAX | fx)
