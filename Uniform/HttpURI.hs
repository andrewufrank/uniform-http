
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
{-# LANGUAGE StandaloneDeriving
--    , GeneralizedNewtypeDeriving
      #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.HttpURI (
    module Uniform.HttpURI
    , module Uniform.Zero
    , module Uniform.Strings
--    , module Network.URI
            )  where


import           Uniform.Error (errorT)
import           Uniform.Strings -- (IsString (..), (</>), (<.>))
import  Uniform.Zero
--import Uniform.HttpCall
--import Uniform.HttpCallWithConduit
import qualified Network.URI as N
--import           Test.Framework
--
--deriving instance Read URI
--deriving instance Read URIAuth

--instance IsString URI where
--    fromString s = read s

localhostTextFile = "http://www.gerastree.at/testaf1" :: Text
-- parseRequest_  does not throw useful exception
-- for the conduit based http

newtype HttpVarParams = HttpVarParams [(Text,Maybe Text)] deriving (Show, Read, Eq )
unHttpVarParams (HttpVarParams p) = p

instance Zeros HttpVarParams where zero = HttpVarParams []

combineHttpVarParams :: HttpVarParams -> HttpVarParams -> HttpVarParams
combineHttpVarParams p1 p2 = HttpVarParams (p11 ++ p22)
        where   p11 = unHttpVarParams p1
                p22 = unHttpVarParams p2

newtype URI = URI N.URI  deriving (Eq)
un2 (URI u) = u   -- to remove the newtype level

parseURI :: Text -> Maybe URI
parseURI t = fmap URI . N.parseURI . t2s $ t

parseAbsoluteURI :: Text -> Maybe URI
parseAbsoluteURI t = fmap URI . N.parseAbsoluteURI . t2s $ t

makeAbsURI :: Text -> URI
makeAbsURI u = URI $ maybe (errorT ["makeURI in LitTypes.ServerNames", u])
                id
                (N.parseAbsoluteURI . t2s   $ u)
makeURI :: Text -> URI
makeURI u = URI $ maybe (errorT ["makeURI in LitTypes.ServerNamess", u])
                id
                (N.parseURI . t2s $ u)

addToURI :: URI -> Text -> URI
-- add a text at end to an URI
addToURI u t =    makeURI $ (uriT u) </> t


addPort2URI :: URI -> Int -> URI
addPort2URI u i = makeURI (uriT u <:> showT i)

uriT :: URI -> Text
-- ^ convert an uri to a text (but not a show instance with "")
uriT = s2t . uriS

uriS :: URI -> String
uriS u =  N.uriToString defaultUserInfoMap (un2 u) $ ""

-- copied
defaultUserInfoMap :: String -> String
defaultUserInfoMap uinf = user++newpass
    where
        (user,pass) = break (==':') uinf
        newpass     = if null pass || (pass == "@")
                                   || (pass == ":@")
                        then pass
                        else ":...@"

instance IsString URI where
    fromString = read . show

instance Show URI where
    showsPrec _ s s2 = (show $ uriS s )++ s2

instance Read URI where
        readsPrec i r =  maybe []  (\res -> [(URI res, rem1)] ) $ N.parseURI x
                where  [(x ::String , rem1)] = readsPrec i r



