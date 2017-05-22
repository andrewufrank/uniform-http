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
import {-@ HTF_TESTS @-} Uniform.HttpGet
------import {-@ HTF_TESTS @-} BuchCode.MarkupText
------import   {-@ HTF_TESTS @-} Lines2para.HandleLayout
------import   {-@ HTF_TESTS @-} Lines2para.Lines2ignore
--import   {-@ HTF_TESTS @-} Lines2para.Lines2para
--import   {-@ HTF_TESTS @-} Parser.ProduceLit
--import   {-@ HTF_TESTS @-} Parser.ProduceNLP   -- calls to NLP
----import   {-@ HTF_TESTS @-} Parser.ConvertTaggerOutput

main =  do
    putStrLn "Lit Text Test.hs:\n"
--    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    r <- htfMain htf_importedTests
    return ()


