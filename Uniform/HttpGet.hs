
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
        let  uri      = dest <> (if vars==[] then   ""
                    else  "/?" <>  urlEncodeVarsT vars)
--                    [("annotators","tokenize,ssplit,pos,lemma,ner,parse")
--                    -- removed ,coref
--                    , ("outputFormat","xml")
--                    ]
        when debugNLPrequest $ putIOwords ["makeHttpPOST7 uri", uri]
        when debugNLPrequest $ putIOwords ["makeHttpPOST7 mimetype", mimetype]
        when debugNLPrequest $ putIOwords ["makeHttpPOST7 body", body ]
        uriEnc <- parseURLchecked uri  -- just testing
        let req = makeHTTPrequest5 POST uri mimetype body
        when debugNLPrequest $ putIOwordsT ["makeHttpPOST7 request", showT req ]

        response <- callHTTP7 False req  -- bool controls debug output
        when debugNLPrequest $ putIOwords ["makeHttpPOST7 response",  response, "from", dest ]

        return response

uriTest = "http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2Cpos%2Clemma%2Cner%2Cparse&outputFormat=xml"
mimetypeTest = "test/application"
bodyTest = "This is a sentence."
res5 = "POST http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2Cpos%2Clemma%2Cner%2\
    \Cparse&outputFormat=xml HTTP/1.1\r\nAccept: */*\r\nContent-Length: 19\r\nContent-Type: \
    \test/application\r\n\r\n"

test_request5 = do
    let req = makeHTTPrequest5 POST uriTest mimetypeTest bodyTest
    assertEqual "" (showT req)




