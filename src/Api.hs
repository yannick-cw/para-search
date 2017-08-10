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
  { tag :: String
  } deriving (Generic)

instance FromJSON SearchRequest
instance ToJSON SearchRequest

data EsSearchResult = EsSearchResult
  { title       :: String
  , imgSrc      :: String
  , href        :: String
  , description :: String
  } deriving (Generic)

instance ToJSON EsSearchResult
instance FromJSON EsSearchResult

data SearchResults = SearchResults
  { searchRequest :: SearchRequest
  , results :: [EsSearchResult]
  } deriving (Generic)

instance ToJSON SearchResults
instance FromJSON SearchResults

type ParaSearchApi = "search" :> ReqBody '[ JSON] SearchRequest :> Post '[ JSON] SearchResults

api :: Proxy ParaSearchApi
api = Proxy
