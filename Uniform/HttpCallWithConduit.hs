
 -----------------------------------------------------------------------------
--
-- Module      :  Uniform.HttpCallWithConduit
--
-- | using http simple to sparql queries and to create requests
-- part of uniform (to use only text
-- uses the newer http-conduit module
-- because teh old HTTP cannot do https

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.HttpCallWithConduit (
    module Uniform.HttpCallWithConduit
    , Http.Request, Http.parseRequest, Http.parseRequest_
--    , Net.RequestMethod (..)  -- for GET, POST
            )  where


import           Uniform.Error
import           Uniform.Strings
--
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding    as E
--
import qualified Network.HTTP.Simple          as Http
import           Network.HTTP.Client          as Client
import           Network.HTTP.Conduit         as Conduit
--import           Network.HTTP.Client.TLS
--import           Network.HTTP.Types.Status  (statusCode)

import Data.Text (take)
import  Test.Framework

type Request2 = Http.Request

callHTTP8get :: Bool -> Text  -> ErrIO  Text
-- call the http-conduit simple for a get
-- see https://haskell-lang.org/library/http-client
callHTTP8get debug dest = do
    req1 <- Http.parseRequest . t2s $ dest
    response <- callIO $  Http.httpLBS req1

    putIOwords ["The status code was: " ,
               showT (Http.getResponseStatusCode response)]
    putIOwords [showT (Http.getResponseHeader "Content-Type" response)]
--    L8.putStrLn $ getResponseBody response
    let res = bb2t . bl2b . Http.getResponseBody $ response :: Text
    -- stops if not an UTF8 encoded text
    putIOwords ["callHTTP8get response: ", res]
    return res

callHTTP8post :: Bool -> Text -> Text -> Text -> Text -> ErrIO Text
-- post a body to the  url given as a type given
--application/sparql-update
callHTTP8post debug appType dest path txt = do
    req1 <- Http.parseRequest . t2s $ dest
    let req2 = Http.setRequestBodyLBS  (b2bl . t2b $ txt)
                $ Http.setRequestHeader "Content-Type" ["application/sparql-update"]
                $ Http.setRequestMethod "POST"
                $ Http.setRequestPath (t2b $ path)
                req1
----            }
    when debug $ putIOwords ["callHTTP8post" , showT req2]
    response <- callIO $  Http.httpLBS req2

    when debug $ putIOwords ["callHTTP8post The status code was: " ,
               showT (Http.getResponseStatusCode response)]
    when debug $ putIOwords [showT (Http.getResponseHeader "Content-Type" response)]
--    L8.putStrLn $ getResponseBody response
    let res = bb2t . bl2b . Http.getResponseBody $ response :: Text
    -- stops if not an UTF8 encoded text
    when debug $ putIOwords ["callHTTP8post response: ", res]
    return res

{-
-- simplified version with more error reported
_callHTTP6 :: Bool -> Http.Request ByteString -> ErrIO  ByteString
-- | executes the HTTP call (e.g. simpleHTTP) and deals with first level of failure
-- the return is a bytstring to decode with the same encoding used for the call
_callHTTP6 debugHTTP request = do
        when debugHTTP $
            putIOwords ["callHTTP6 : "
                    , "\nrequest", showT request
                    , "requestbody",  bb2t $ Net.rqBody request]
        res <- callIO $
                do
                    res1 <- Http.simpleHTTP request
                    when debugHTTP $
                        putIOwords ["callHTTP6 result is is", showT res1]
                    return res1
                 `catchError` \e -> do
                             putIOwords ["callHTTP6 http.simpleHTTP error caught 3", showT e
                                    , "\n should not occur - caught by callIO ??"
                                    , "\n note hint: replace localhost by 127.0.0.1"
                                    ,  "\n", showT request]
                             fail . unwords $  [ "callHTTP6 httperror 3", show e]
                                             -- is in the IO monad, not ErrIO
        case res of
             Left msg -> do
                     putIOwords ["callHTTP6 reported error ", showT msg, "for request \n", showT request]
                     throwErrorT ["callHTTP6 http select 2", showT msg, "for request \n", showT request]
             Right response -> do
                     when debugHTTP $ putIOwords ["callHTTP6 produced response 1"]
                     res3 <- callIO $ do
                                 code1 <- Http.getResponseCode res
                                 when debugHTTP $ putIOwords ["callHTTP6  getResponseCode 6", showT code1]
                                 body1 <- Http.getResponseBody res
                                 let body2 =  body1  -- change for 6
                                 when debugHTTP $ putIOwords ["callHTTP6  getResponseBody 6", bb2t body2]
                                 if fst3 code1 == 2
                                    then do
                                         when debugHTTP $ putIOwords ["callHTTP6  return ok with body", showT code1, bb2t body2]
                                         return . Right $ body2 --
--                                                   -- the result is parsed! $ unwords' [showT res, "with body", body2]
                                    else do
                                         putIOwords ["callHTTP6  return Left code", showT code1
                                                        , "for request \n", showT request]
                                         return . Left . unwords $ ["callHTTP6 returns code", show  code1, bb2s body2
                                                    , "for request \n", show  request]
                              `catchError` \e -> do
                                 putIOwords ["callHTTP6 error in getResponseBody 5 100 char"
                                        , Data.Text.take 100 . showT $ e, "for request \n", showT request]
                                 return . Left . unwords $   [ "callHTTP6 httperror 5", show e
                                                , "for request \n", show  request]
                     case res3 of
                        Left msg2 -> throwError . s2t $ msg2
                        Right b -> return (b ::ByteString)

     `catchError` (\e -> do
             putIOwords ["callHTTP6 error caught 7",  e, "for request \n", showT request] -- " showT msg])
             putIOwords ["callHTTP6 error caught 7",  "requestbody",  bb2t $ Net.rqBody request ] -- " showT msg])
             throwError e
                )


type URItext = Text

makeHTTPrequest5 :: Http.RequestMethod -> NetURI.URI -> Text -> Text -> Net.Request ByteString
-- a call to make a HTTP request with method to the URI (text)
-- content type and body
-- does not work for queries with no body
makeHTTPrequest5 method uri contentType body =
    Net.Request { Net.rqURI = uri
--                    fromJustNoteT ["makeHTTPrequest5 parseURI", showT uri]
--                        . NetURI.parseURI . t2s $ uri
             , Net.rqHeaders =   [hAccept, hLength , hContentType]
             , Net.rqMethod = method
             , Net.rqBody =   body'

             } -- :: Net.Request.ByteString
    where
        hAccept = Net.mkHeader Net.HdrAccept "*/*" -- "application/x-www-form-urlencoded"
--                                  "application/sparql-results+xml"
        hLength =  Net.mkHeader  Net.HdrContentLength $ show ( B.length body')
        hContentType = Net.mkHeader Net.HdrContentType  (t2s contentType)
        body' = E.encodeUtf8  body -- Net.urlEncode $ t2s body -- error 409 invalid path
        -- was just t2s body --


makeHTTPgetrequestNoBody :: URItext -> Text -> Text -> Net.Request String
makeHTTPgetrequestNoBody uri argument text =
    Net.getRequest  $ concat [ t2s uri , t2s argument , Net.urlEncode . t2s $ text]




parseURLchecked ::  Text -> ErrIO NetURI.URI
parseURLchecked uri = do
    let  urx = NetURI.parseURI $ t2s uri
    case urx of
            Nothing ->  throwErrorT ["URLerror" , "not a proper url " ,  uri]
            Just uriEnc -> return uriEnc

urlEncodeVarsT:: [(Text,Text)] -> Text
urlEncodeVarsT = s2t . Net.urlEncodeVars . map (pair t2s)

urlEncode :: Text -> Text
urlEncode = s2t . Net.urlEncode . t2s

destTestFail = "127.0.0.1:9000"
destTest9001 = "http://127.0.0.1:9001"

test_parseURI = assertEqual "Just http://127.0.0.1:9001" (showT . NetURI.parseURI $ destTest9001)
test_parseURI_fail  = assertEqual "Nothing" (showT . NetURI.parseURI $ destTestFail)

uriTest = "http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2Cpos%2Clemma%2Cner%2Cparse&outputFormat=xml"

mimetypeTest = "test/application"
bodyTest = "This is a sentence."
res5 = "POST http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2Cpos%2Clemma%2Cner%2\
    \Cparse&outputFormat=xml HTTP/1.1\r\nAccept: */*\r\nContent-Length: 19\r\nContent-Type: \
    \test/application\r\n\r\n"

test_request5 = do
    let uri1 = (NetURI.parseURI uriTest)  :: Maybe (NetURI.URI)
    let req = makeHTTPrequest5 Net.POST (fromJustNote "x" uri1) mimetypeTest bodyTest
    assertEqual res5 (showT req)

--test_fromJust_givesError = assertEqual 1 (fromJustNote "test_fromJust" (Nothing ::Maybe Int))

--test_request5_error = do
--    let uri1 = (NetURI.parseURI destTestFail)  :: Maybe (NetURI.URI)
--    let req = makeHTTPrequest5 Net.POST (fromJustNote "x" uri1) mimetypeTest bodyTest
--    assertEqual res5 (showT req)

-- todo error or strings
-- fromJustNote'= fromJustNote
-- fromJustNote' msg mb = case mb of
--                             Just r -> r
--                             Nothing -> errorT ["fromJust at ", msg , "with arg", showT mb]
-}