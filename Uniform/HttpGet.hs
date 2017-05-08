
 -----------------------------------------------------------------------------
--
-- Module      :  Store.RDFstore.HttpCall
--
-- | using http simple to sparql queries and to create requests
-- part of uniform (to use only text

-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.HttpGet (makeHttpPost7

            )  where


import           Uniform.Error
import           Uniform.Strings
import Uniform.HttpCall
--

makeHttpPost7 :: Bool -> Text -> [(Text,Text)] -> Text -> Text  -> ErrIO  Text
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
        when debugNLPrequest $ putIOwords ["makeHttpPOST7 uri", uri ]
        uriEnc <- parseURLchecked uri  -- just testing
        let req = makeHTTPrequest5 POST uri mimetype body
        when debugNLPrequest $ putIOwordsT ["makeHttpPOST7 request", showT req ]

        response <- callHTTP7 False req  -- bool controls debug output
        when debugNLPrequest $ putIOwords ["makeHttpPOST7 response",  response, "from", dest ]

        return response
