{-# LANGUAGE TemplateHaskell #-}
-- TEMPLATE HASKELL SPLICES DON'T WORK CURRENTLY. SEE: #32
module TH.Splice.Use where

import TH.Splice.Define

$(def "x")

$(defHello)

$defHello2