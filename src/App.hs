{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module App where

import           Api
import           EsSearch                 (search)
import           Network.Wai.Handler.Warp
import           Prelude                  ()
import           Prelude.Compat
import           Servant
import           System.IO.Error
import           Control.Monad.IO.Class        (liftIO)


paraServer :: Server ParaSearchApi
paraServer = search'

search' :: SearchRequest -> Handler SearchResults
search' request = liftIO (search (query request))

startServer :: IO ()
startServer =
  catchIOError
    (do _ <- print "running on 8082"
        run 8082 $ serve api paraServer)
    (print . (++) "failed startup with: " . show)
