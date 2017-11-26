
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

module Uniform.HttpURI (
    module Uniform.HttpURI
    , module Network.URI
            )  where


import           Uniform.Error
import           Uniform.Strings
--import Uniform.HttpCall
--import Uniform.HttpCallWithConduit
import Network.URI
import           Test.Framework
--



localhostTextFile = "http://www.gerastree.at/testaf1" :: Text
-- parseRequest_  does not throw useful exception
-- for the conduit based http


makeAbsURI :: Text -> URI
makeAbsURI u = maybe (errorT ["makeURI in Producer.Servers", u])
                id
                (parseAbsoluteURI . t2s $ u)
makeURI :: Text -> URI
makeURI u = maybe (errorT ["makeURI in Producer.Servers", u])
                id
                (parseURI . t2s $ u)

addToURI :: URI -> Text -> URI
-- add a text at end to an URI
addToURI u t =    makeURI $ (showT u) </> t

test_add2uri = assertEqual "http://nlp.gerastree.at:9001/xtestx" (showT $ addToURI destTest9001g "xtestx")
test_add2uri2 = assertEqual "http://127.0.0.1/xtestx" (showT $ addToURI forportTest "xtestx")

addPort2URI :: URI -> Int -> URI
addPort2URI u i = makeURI (showT u <:> showT i)


destTestFailx = "127.0.0.1:9000" ::Text  -- missing http://
destTest9001g = makeAbsURI "http://nlp.gerastree.at:9001"
destTest9000e = makeAbsURI "http://nlp.gerastree.at:9000"

test_makeURIok = assertEqual "http://nlp.gerastree.at:9001" (showT   destTest9001g)
test_makeURIfail = do
            res <- mustError "test failing uri construction"
                                $ return $ makeAbsURI  destTestFailx
            assertBool (not res)
--test_makeURIfail2 = assertEqual "Nothing" (showT $ makeAbsURI  destTestFailx)


test_addport = assertEqual "http://127.0.0.1:9001" (showT $ addPort2URI forportTest 9001)

-- todo move to error
-- does not work in the above situation
mustError :: MonadError m => Text -> m a -> m Bool
mustError msg f = do
                        f
                        return False
               `catchError` \e -> return True

forportTest :: URI
forportTest = makeURI "http://127.0.0.1"

uriTest = "http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2Cpos%2Clemma%2Cner%2Cparse&outputFormat=xml"
res5 = "POST http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2Cpos%2Clemma%2Cner%2\
    \Cparse&outputFormat=xml HTTP/1.1\r\nAccept: */*\r\nContent-Length: 19\r\nContent-Type: \
    \test/application\r\n\r\n"

