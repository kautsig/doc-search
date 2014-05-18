{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections, TypeOperators #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.Maybe
import Data.Text (Text)
import Data.Text.Lazy (unpack, replace, toStrict)
import Data.Conduit
import Data.Aeson
import qualified Data.Map as M

import Happstack.Lite
import Happstack.Server.Monads
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Network.HTTP.Conduit hiding (Response)
import qualified Data.ByteString.Lazy as B

import GHC.Generics

import EsRequest
import EsResponse

-- Define where to get the data from

jsonURL :: String
jsonURL = "http://localhost:9200/doctors/_search"

doQuery :: B.ByteString -> IO B.ByteString
doQuery body = runResourceT $ do
    manager <- liftIO $ newManager conduitManagerSettings
    request <- parseUrl jsonURL
    let postRequest = request { Network.HTTP.Conduit.method = "POST",
                                Network.HTTP.Conduit.requestBody = RequestBodyLBS body }
    responseBody <$> httpLbs postRequest manager

-- Query the server and decode the result

buildQuery :: [(String, String)] -> B.ByteString
buildQuery [] = encode queryRequest
buildQuery t = encode $ queryRequestWithParams t

getResult :: [(String, String)] -> IO (Either String QueryResult)
getResult t = (eitherDecode <$> doQuery (buildQuery t)) :: IO (Either String QueryResult)

-- Debug functions for analyzing request/response

dumpRequest :: IO ()
dumpRequest = B.writeFile "request.dump"  $ encode queryRequest

dumpResponse :: IO ()
dumpResponse = do
  response <- liftIO $ doQuery $ buildQuery [("insurance", "GKK")]
  B.writeFile "response.dump" response

-- Rendering functions

renderMenu :: Either String QueryResult -> Html
renderMenu x = case x of
    Left err -> H.p "Error"
    Right res -> H.div $ renderResultFacets $ facetsL res

renderResultFacets :: M.Map String ResultFacet -> Html
renderResultFacets f = H.div $ forM_ (M.toList f) renderResultFacet

renderResultFacet :: (String, ResultFacet) -> Html
renderResultFacet f = do
  H.div $ do
     "Total: "
     toHtml . totalF $ snd f
     ", "
     "Other: "
     toHtml . otherF $ snd f
  H.ul ! A.class_ "nav nav-sidebar"  $ forM_ (termsF $ snd f) (H.li . renderFacetTerm (fst f))

renderFacetTerm :: String -> ResultFacetTerm -> Html
renderFacetTerm n t = H.a ! A.href (H.toValue ("/?" ++ n ++  "=" ++ termC t)) $ do
  H.toHtml $ termC t
  " ("
  H.toHtml $ countC t
  ")"

renderResult :: Either String QueryResult -> Html
renderResult x = case x of
  Left err -> H.p "Error"
  Right res -> H.div $ do
                 renderResultStats $ hits res
                 renderResultList $ hitsL $ hits res

renderResultStats :: Hits -> Html
renderResultStats h = H.div $ do
  "Total Hits: "
  toHtml . total $ h
  ", Max Score: "
  toHtml . max_score $ h

renderResultList :: [Hit] -> Html
renderResultList h = H.div $ H.table ! A.class_ "table table-striped" $ forM_ h (H.tr . renderHitRow)

renderHitRow :: Hit -> Html
renderHitRow h = renderDoctorRow $ _source h

renderDoctorRow :: Doctor -> Html
renderDoctorRow d = do
  H.td ! A.style "width: 20px;" $ toHtml . show $ objectId d
  renderMaybeCell (title d) ! A.style "width: 20px;"
  renderMaybeCell $ honoraryTitle d
  renderMaybeCell (degree d) ! A.style "width: 80px;"
  renderMaybeCell $ firstName d
  renderMaybeCell $ lastName d
  renderMaybeCell $ street $ address d
  renderMaybeCell $ zipCode $ address d
  renderMaybeCell $ city $ address d

renderMaybeCell :: Maybe String -> Html
renderMaybeCell ms =
  case ms of
    Just a -> H.td $ toHtml a
    Nothing -> H.td ""

-- The webserver part

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum [ dir "files" fileServing
             , homePage
             ]

template :: Html -> Html -> Response
template menu body = toResponse $
  H.html $ do
    H.head $ do
      H.link ! A.rel "stylesheet" ! A.href "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
      H.link ! A.rel "stylesheet" ! A.href "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css"
      H.link ! A.rel "stylesheet" ! A.href "/files/dashboard.css"
      H.script ! A.src "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js" $ ""
    H.body ! A.style "padding-top: 50px;" $ do
      H.div ! A.class_ "navbar navbar-inverse navbar-fixed-top" $
        H.div ! A.class_ "container-fluid" $
          H.p $ H.a ! A.class_ "navbar-brand" ! A.href "/" $ "Doctor search using haskell and elastic search"
      H.div ! A.class_ "container-fluid" $
        H.div ! A.class_ "row" $ do
          H.div ! A.class_ "col-sm-4 col-md-3 sidebar" $ menu
          H.div ! A.class_ "col-sm-9 col-sm-offset-4 col-md-9 col-md-offset-3 main" $ body

homePage :: ServerPartT IO Response
homePage = do
  -- extract the request params
  insurance <- optional $ lookTexts "insurance"
  discipline <- optional $ lookTexts "discipline"
  zipCode <- optional $ lookTexts "zipCode"

  -- build a list of tuples e.g. [("insurance", "GKK")]
  let params = [(a, b) | a <- ["insurance"], b <- map unpack (fromJust insurance)]
               ++ [(a, b) | a <- ["discipline"], b <- map unpack (fromJust discipline)]
               ++ [(a, b) | a <- ["zip"], b <-  map unpack (fromJust zipCode)]

  -- use the list to get the results
  result <- liftIO $ getResult params
  ok $ template (renderMenu result) $ do
    H.h2 ! A.class_ "page-header" $ "Result"
    renderResult result

fileServing :: ServerPart Response
fileServing = serveDirectory EnableBrowsing ["dashboard.css"] "files"
