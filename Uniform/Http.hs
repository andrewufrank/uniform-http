
 -----------------------------------------------------------------------------
--
-- Module      :  Uniform.HttpCall
--
-- | the only externally visible module
-- exports all
-- using http simple to sparql queries and to create requests
-- part of uniform (to use only text
-- uses the newer http-conduit module
-- because teh old HTTP cannot do https

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveAnyClass,
  RecordWildCards #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Uniform.Http (TimeOutSec, mkTimeOut, mkTimeOutDefault
        , mkServerURI, ServerURI
        , URI, HttpVarParams
        , uriT  -- required? 
        , mkAppType, AppType 
    , callHTTP10post
    , mkHttpPath, HttpPath
    , mkHttpVarParams, HttpVarParams 
    , module Uniform.Error
            )  where

import           Uniform.Error
import Uniform.HttpCall
import Uniform.HttpURI



