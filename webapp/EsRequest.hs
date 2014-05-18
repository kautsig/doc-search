{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections, DeriveGeneric #-}

module EsRequest where

import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M

import GHC.Generics


-- Request data structure

data QueryRequest = QueryRequest {
  sizeR :: Int,
  query :: Query,
  facets :: M.Map String RequestFacet
} deriving (Eq, Show, Generic)

instance ToJSON QueryRequest where
  toJSON o = object [
    "size" .= sizeR o,
    "query" .= query o,
    "facets" .= facets o ]

-- Single query, consisting of search and filters

data Query = Query {
  match_all :: EmptyObject,
  filtered :: Maybe FilteredQuery
} deriving (Eq, Show, Generic)

instance ToJSON Query where
    toJSON (Query manf optf) = object $ catMaybes
        [ ("match_all" .=) <$> pure manf
        , ("filtered" .=) <$> optf ]

-- Helper to pass an empty object to the match_all filter

data EmptyObject = EmptyObject {
} deriving (Eq, Show, Generic)

instance ToJSON EmptyObject


-- The filter part of the query

data FilteredQuery = FilteredQuery {
  filterQ :: AndFilter
} deriving (Eq, Show, Generic)

instance ToJSON FilteredQuery where
  toJSON o = object [
    "filter" .= filterQ o ]

-- A filter combining two other filters using AND

data AndFilter = AndFilter {
  andF :: [TermFilter]
} deriving (Eq, Show, Generic)

instance ToJSON AndFilter where
  toJSON o = object [
    "and" .= andF o ]

-- A filter restricting by search term

data TermFilter = TermFilter {
  termT :: M.Map String String
} deriving (Eq, Show, Generic)

instance ToJSON TermFilter where
  toJSON o = object [
    "term" .= termT o ]

-- Data objects for facet configuration

data RequestFacet = RequestFacet {
  terms :: RequestFacetTerms
} deriving (Eq, Show, Generic)

instance ToJSON RequestFacet

data RequestFacetTerms = RequestFacetTerms {
  field :: String,
  size :: Int
} deriving (Eq, Show, Generic)

instance ToJSON RequestFacetTerms

-- Build a template for a default request

-- Facet configuration

disciplineFacet :: RequestFacet
disciplineFacet = RequestFacet {
  terms = RequestFacetTerms {
    field = "discipline",
    size = 40
  }
}

zipFacet :: RequestFacet
zipFacet = RequestFacet {
  terms = RequestFacetTerms {
    field = "address.zip",
    size = 10
  }
}

insuranceFacet :: RequestFacet
insuranceFacet = RequestFacet {
  terms = RequestFacetTerms {
    field = "insurance",
    size = 10
  }
}

requestFacets :: M.Map String RequestFacet
requestFacets = M.fromList [
  ("discipline", disciplineFacet),
  ("zip", zipFacet),
  ("insurance", insuranceFacet)]

-- Filter configuration

matchAllQuery :: Query
matchAllQuery = Query {
  match_all = EmptyObject {},
  filtered = Nothing
}

-- Put facet configuration and filter into 1 request

queryRequest :: QueryRequest
queryRequest = QueryRequest {
  sizeR = 50,
  query = matchAllQuery,
  facets = requestFacets
}


queryRequestWithParams :: [(String, String)] -> QueryRequest
queryRequestWithParams l = do
  QueryRequest {
    sizeR = 50,
    query = Query {
      match_all = EmptyObject {},
      filtered = Just FilteredQuery {
        filterQ = AndFilter {
          andF = map termf l
        }
      }
    },
    facets = requestFacets
  }

termf :: (String, String) -> TermFilter
termf t = TermFilter {
  termT = M.fromList [t]
}
