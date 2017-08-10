{-# LANGUAGE OverloadedStrings #-}

module EsSearch
  ( search
  ) where

import           Api
import           Control.Monad.IO.Class        (liftIO)
import           Data.Maybe                    (maybeToList)
import           Data.Text                     (pack)
import           Database.V5.Bloodhound
import           Database.V5.Bloodhound.Client (parseEsResponse)
import           Database.V5.Bloodhound.Types  (EsResult (..))
import           Network.HTTP.Client           (defaultManagerSettings)

search :: String -> IO [EsSearchResult]
search searchString =
  runBH' $ do
    reply <- searchByType indexName mapping search
    searchResult <- parse reply
    return $ extractSearchResult searchResult
  where
    testServer = Server "http://localhost:9200"
    runBH' = withBH defaultManagerSettings testServer
    indexName = IndexName "offer"
    mapping = MappingName "offers"
    boost = Nothing
    fields = [FieldName "title", FieldName "description"]
    query = QueryMultiMatchQuery $ mkMultiMatchQuery fields (QueryString $ pack searchString)
    search = mkSearch (Just query) boost

parse :: Reply -> BH IO (SearchResult EsSearchResult)
parse reply = do
  searchResult <- parseEsResponse reply
  case searchResult of
--  todo: error handling
    Left err            -> undefined
    Right searchResults -> return searchResults

extractSearchResult :: SearchResult EsSearchResult -> [EsSearchResult]
extractSearchResult res = fmap hitSource (hits $ searchHits res) >>= maybeToList
