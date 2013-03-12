{-# LANGUAGE OverloadedStrings #-}
module Zippy.Tasks.Data.Relationships where
import Data.Proxy
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Riak.Content (Index(..), Link(..))
import Zippy.Tasks.Data.Types

task :: Proxy Task
task = Proxy

list :: Proxy List
list = Proxy

listIx :: Key List -> Index
listIx = keyIndex "list"
