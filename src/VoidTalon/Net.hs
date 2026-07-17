module VoidTalon.Net (checkStatusOK) where

import Control.Monad (unless)
import Network.HTTP.Client (Response (responseStatus))
import Network.HTTP.Types (statusIsSuccessful)

checkStatusOK :: (MonadFail m) => Response a -> m ()
checkStatusOK res =
  unless (statusIsSuccessful status) $
    fail $
      "Request failed with status code " <> (show status)
  where
    status = res.responseStatus
