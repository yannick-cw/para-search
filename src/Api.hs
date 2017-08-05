{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Api where

import           Data.Aeson.Types
import           GHC.Generics
import           Prelude          ()
import           Prelude.Compat
import           Servant

newtype SearchRequest = SearchRequest
  { query :: String
  } deriving (Generic)

instance FromJSON SearchRequest

data EsSearchResult = EsSearchResult
  { title       :: String
  , imgSrc      :: String
  , href        :: String
  , description :: String
  } deriving (Generic)

instance ToJSON EsSearchResult
instance FromJSON EsSearchResult

newtype SearchResults = SearchResults
  { results :: [EsSearchResult]
  } deriving (Generic)

instance ToJSON SearchResults
instance FromJSON SearchResults

type ParaSearchApi = "search" :> ReqBody '[ JSON] SearchRequest :> Post '[ JSON] SearchResults

api :: Proxy ParaSearchApi
api = Proxy
