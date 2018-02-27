
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

module Uniform.HttpURI_test where


import           Uniform.Error
import           Uniform.Strings
--import Uniform.HttpCall
--import Uniform.HttpCallWithConduit
--import Network.URI
import           Test.Framework
import Uniform.HttpURI

--

test_add2uri = assertEqual "http://nlp.gerastree.at:9001/xtestx" (showT $ addToURI destTest9001g "xtestx")
test_add2uri2 = assertEqual "http://127.0.0.1/xtestx" (showT $ addToURI forportTest "xtestx")




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

test_parseURI = assertEqual "Just http://127.0.0.1:9001" (showT . NetURI.parseURI $ destTest9001)
test_parseURI_fail  = assertEqual "Nothing" (showT . NetURI.parseURI $ destTestFail)
