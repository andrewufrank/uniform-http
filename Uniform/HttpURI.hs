
 -----------------------------------------------------------------------------
--
-- Module      :  Store.RDFstore.HttpCall
--
-- | using http simple to sparql queries and to create requests
-- part of uniform (to use only text
-- wraps URI in URI

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
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , DeriveAnyClass
      #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.HttpURI (
        -- TimeOutSec, mkTimeOut, mkTimeOutDefault
        -- , URI, HttpQueryParams
    module Uniform.HttpURI
    , module Uniform.Zero
    , module Uniform.Strings
--    , module Network.URI
            )  where


import           Uniform.Error (errorT)
import           Uniform.Strings -- (IsString (..), (</>), (<.>)) 
import  Uniform.Zero
import qualified Network.URI as N

-- a server URI (not including the port, but absolute)
newtype ServerURI = ServerURI {unServerURI :: URI}
                deriving (Show, Read, Eq, Ord, Generic, Zeros)
mkServerURI :: Text -> ServerURI 
mkServerURI = ServerURI . makeAbsURI 

-- | a type for the application path when calling Http 
-- after the URI till the ? (starts with /)
newtype HttpPath = HttpPath Text 
    deriving (Show, Read, Eq, Ord, Generic, Zeros)
mkHttpPath = HttpPath    -- could check for acceptance here? 


-- | a timeout in seconds 
newtype TimeOutSec = TimeOutSec (Maybe Int)
    deriving (Eq, Ord, Show, Read, Generic, Zeros)
mkTimeOut i = TimeOutSec (Just i)
mkTimeOutDefault = TimeOutSec Nothing

-- | a special type for the app type argumetn 
newtype AppType = AppType Text 
    deriving (Eq, Ord, Show, Read, Generic, Zeros)
mkAppType = AppType 

-- | the type for the paramter key - value pairs, comes after the ? 
newtype HttpQueryParams = HttpQueryParams [(Text, Maybe Text)]
    deriving (Show, Read, Eq, Generic, Zeros)
unHttpQueryParams (HttpQueryParams p) = p
mkHttpQueryParams = HttpQueryParams 
--instance Zeros HttpQueryParams where zero = HttpQueryParams []
-- unclear why automatic derivation does not work

combineHttpQueryParams :: HttpQueryParams -> HttpQueryParams -> HttpQueryParams
combineHttpQueryParams p1 p2 = HttpQueryParams (p11 ++ p22)
        where   p11 = unHttpQueryParams p1
                p22 = unHttpQueryParams p2

newtype URI = URI N.URI  deriving (Eq, Ord, Generic)
un2 (URI u) = u   -- to remove the newtype level
instance Zeros URI where
    zero = makeURI ""

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

newtype PortNumber = PortNumber Int 
    deriving (Eq, Ord, Show, Read, Generic, Zeros)
mkPortNumber = PortNumber 

addPort2ServerURI :: ServerURI -> PortNumber -> ServerURI
addPort2ServerURI (ServerURI u) (PortNumber i) = mkServerURI (uriT u <:> showT i)

uriT :: URI -> Text
-- ^ convert an uri to a text (but not a show instance with "")
uriT = s2t . uriS

uriS :: URI -> String
uriS u =  N.uriToString defaultUserInfoMap (un2 u) $ ""

-- copied
defaultUserInfoMap :: String -> String
defaultUserInfoMap uinf = user ++ newpass
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



