{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections, DeriveGeneric #-}

module EsResponse where

import Control.Monad
import Control.Applicative

import Data.Aeson
import qualified Data.Map as M

import GHC.Generics

-- Query result data structure

data QueryResult = QueryResult {
  hits :: Hits,
  facetsL :: M.Map String ResultFacet
} deriving (Eq, Show, Generic)

instance FromJSON QueryResult where
  parseJSON (Object v) =
    QueryResult <$> v .: "hits"
                <*> v .: "facets"

data ResultFacet = ResultFacet {
  totalF :: Int,
  otherF :: Int,
  termsF :: [ResultFacetTerm]
} deriving (Eq, Show, Generic)

instance FromJSON ResultFacet where
  parseJSON (Object v) =
    ResultFacet <$> v .: "total"
                <*> v .: "other"
                <*> v .: "terms"


data ResultFacetTerm = ResultFacetTerm {
  termC :: String,
  countC :: Int
} deriving (Eq, Show, Generic)

instance FromJSON ResultFacetTerm where
  parseJSON (Object v) =
    ResultFacetTerm <$> v .: "term"
                    <*> v .: "count"


-- All hits
data Hits = Hits {
  total :: Int,
  max_score :: Double,
  hitsL :: [Hit]
} deriving (Eq, Show, Generic)

instance FromJSON Hits where
  parseJSON (Object v) =
    Hits <$> v .: "total"
         <*> v .: "max_score"
         <*> v .: "hits"

-- A single hit
data Hit = Hit {
  _index :: String,
  _type :: String,
  _id :: String,
  _source :: Doctor
} deriving (Eq, Show, Generic)

instance FromJSON Hit

-- Doctor
data Doctor = Doctor {
  objectId :: Int,
  discipline :: Maybe String,
  title :: Maybe String,
  honoraryTitle :: Maybe String,
  degree :: Maybe String,
  firstName :: Maybe String,
  lastName :: Maybe String,
  address :: Address,
  insurance :: Maybe [String]

} deriving (Eq, Show, Generic)

instance FromJSON Doctor where
  parseJSON (Object v) =
    Doctor <$> v .: "objectId"
           <*> v .:? "discipline"
           <*> v .:? "title"
           <*> v .:? "honoraryTitle"
           <*> v .:? "degree"
           <*> v .:? "firstName"
           <*> v .:? "lastName"
           <*> v .: "address"
           <*> v .:? "insurance"
  parseJSON _ = mzero

-- Address
data Address = Address {
  street :: Maybe String,
  zipCode :: Maybe String,
  city :: Maybe String,
  phone :: Maybe String,
  fax :: Maybe String,
  email :: Maybe String
} deriving (Eq, Show, Generic)

instance FromJSON Address where
  parseJSON (Object v) =
    Address <$> v .:? "street"
            <*> v .:? "zip"
            <*> v .:? "city"
            <*> v .:? "phone"
            <*> v .:? "fax"
            <*> v .:? "email"
