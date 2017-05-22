
 -----------------------------------------------------------------------------
--
-- Module      :  Store.RDFstore.HttpCall
--
-- | using http simple to sparql queries and to create requests
-- part of uniform (to use only text

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

module Uniform.HttpGet (
    module Uniform.HttpGet
            )  where


import           Uniform.Error
import           Uniform.Strings
import Uniform.HttpCall

import           Test.Framework
--

makeHttpPost7 :: Bool -> Text -> [(Text,Text)] -> Text -> Text
            -> ErrIO  Text
-- make a POST request to coreNLP server
-- call and return with text
makeHttpPost7 debugNLPrequest dest vars mimetype  body = do
        -- when debugNLPrequest $
        -- putIOwords ["makeHttpPOST7", dest]
        when debugNLPrequest $ putIOwords ["\twith \n", body]
        let  uritext = dest <> (if null vars then   ""
                    else  "/?" <>  urlEncodeVarsT vars)
--                    [("annotators","tokenize,ssplit,pos,lemma,ner,parse")
--                    -- removed ,coref
--                    , ("outputFormat","xml")
--                    ]
        when debugNLPrequest $ putIOwords ["makeHttpPOST7 uritext", uritext]
        when debugNLPrequest $ putIOwords ["makeHttpPOST7 mimetype", mimetype]
        when debugNLPrequest $ putIOwords ["makeHttpPOST7 body", body ]
        uriEnc <- parseURLchecked uritext  -- just testing
                    -- seems not to do much
--        let muri = parseURI uritext
--        case muri of
--            Nothing -> throwErrorT ["makeHttpPOST7 - uri not ok", uritext]
--            Just uri -> do
        let uri = uriEnc
        when debugNLPrequest $ putIOwords ["makeHttpPOST7 uri", showT uri]
        let req = makeHTTPrequest5 POST uri mimetype body
        when debugNLPrequest $ putIOwordsT ["makeHttpPOST7 request", showT req ]

        response <- callHTTP7 False req  -- bool controls debug output
        when debugNLPrequest $ putIOwords ["makeHttpPOST7 response"
                                ,  response, "from", dest ]

        return response

uriTest = "http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2Cpos%2Clemma%2Cner%2Cparse&outputFormat=xml"
--uriTestFail = " 127.0.0.1:9001/?annotators=tokenize%2Cssplit%2Cpos%2Clemma%2Cner%2Cparse&outputFormat=xml"
--mimetypeTest = "test/application"
--bodyTest = "This is a sentence."
res5 = "POST http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2Cpos%2Clemma%2Cner%2\
    \Cparse&outputFormat=xml HTTP/1.1\r\nAccept: */*\r\nContent-Length: 19\r\nContent-Type: \
    \test/application\r\n\r\n"

--
destTest9001g = "http://127.0.0.1:9001"
destTest9000e = "http://127.0.0.1:9000"
varsTest = [("annotators", "tokenize,pos")]
test_makePost7german = do
    response <- runErr $ makeHttpPost7 True  destTest9001g varsTest mimetypeTest bodyTest
    assertEqual res7g  response

res7g =
    Right "{\"sentences\":[{\"index\":0,\"tokens\":[{\"index\":1,\"word\":\"This\",\"originalText\":\"This\",\"characterOffsetBegin\":0,\"characterOffsetEnd\":4,\"pos\":\"FM\",\"before\":\"\",\"after\":\" \"},{\"index\":2,\"word\":\"is\",\"originalText\":\"is\",\"characterOffsetBegin\":5,\"characterOffsetEnd\":7,\"pos\":\"FM\",\"before\":\" \",\"after\":\" \"},{\"index\":3,\"word\":\"a\",\"originalText\":\"a\",\"characterOffsetBegin\":8,\"characterOffsetEnd\":9,\"pos\":\"FM\",\"before\":\" \",\"after\":\" \"},{\"index\":4,\"word\":\"sentence\",\"originalText\":\"sentence\",\"characterOffsetBegin\":10,\"characterOffsetEnd\":18,\"pos\":\"FM\",\"before\":\" \",\"after\":\"\"},{\"index\":5,\"word\":\".\",\"originalText\":\".\",\"characterOffsetBegin\":18,\"characterOffsetEnd\":19,\"pos\":\"$.\",\"before\":\"\",\"after\":\"\"}]}]}"

test_makePost7english = do
    response <- runErr $ makeHttpPost7 True  destTest9000e varsTest mimetypeTest bodyTest
    assertEqual res7g  response

res7e =
    Right "{\"sentences\":[{\"index\":0,\"tokens\":[{\"index\":1,\"word\":\"This\",\"originalText\":\"This\",\"characterOffsetBegin\":0,\"characterOffsetEnd\":4,\"pos\":\"DT\",\"before\":\"\",\"after\":\" \"},{\"index\":2,\"word\":\"is\",\"originalText\":\"is\",\"characterOffsetBegin\":5,\"characterOffsetEnd\":7,\"pos\":\"VBZ\",\"before\":\" \",\"after\":\" \"},{\"index\":3,\"word\":\"a\",\"originalText\":\"a\",\"characterOffsetBegin\":8,\"characterOffsetEnd\":9,\"pos\":\"DT\",\"before\":\" \",\"after\":\" \"},{\"index\":4,\"word\":\"sentence\",\"originalText\":\"sentence\",\"characterOffsetBegin\":10,\"characterOffsetEnd\":18,\"pos\":\"NN\",\"before\":\" \",\"after\":\"\"},{\"index\":5,\"word\":\".\",\"originalText\":\".\",\"characterOffsetBegin\":18,\"characterOffsetEnd\":19,\"pos\":\".\",\"before\":\"\",\"after\":\"\"}]}]}"

destTestFailx = "127.0.0.1:9000"  -- missing http://

test_makePost7englishFail = do
    response <- runErr $ makeHttpPost7 True  destTestFailx varsTest mimetypeTest bodyTest
    assertEqual (Left "URLerror not a proper url  127.0.0.1:9000/?annotators=tokenize%2Cpos")  response

--test_parseURI = assertEqual "Just http://127.0.0.1:9001" (showT . NetURI.parseURI $ destTest9001)
--test_parseURI_fail  = assertEqual "Nothing" (showT . NetURI.parseURI $ destTestFail)

