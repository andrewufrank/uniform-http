
 -----------------------------------------------------------------------------
--
-- Module      :  Store.RDFstore.HttpCall
--
-- | using http simple to sparql queries and to create requests
-- part of uniform (to use only text

-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

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
--import           Test.Framework
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


addPort2URI :: URI -> Int -> URI
addPort2URI u i = makeURI (showT u <:> showT i)






