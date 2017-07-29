-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies

    #-}

module Main where

import Uniform.Strings
import           Test.Framework
import {-@ HTF_TESTS @-} Uniform.HttpCallWithConduit
import {-@ HTF_TESTS @-} Uniform.HttpURI

main =  do
    putStrLn "Lit Text Test.hs:\n"
--    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    r <- htfMain htf_importedTests
    return ()


