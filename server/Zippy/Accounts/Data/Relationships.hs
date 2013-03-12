{-# LANGUAGE OverloadedStrings #-}
module Zippy.Accounts.Data.Relationships (
    Group,
    User,
    assignedToIx,
    creatorIx,
    group,
    memberLink,
    ownerIx,
    ownerLink,
    user
) where
import Data.Proxy
import Zippy.Accounts.Data.Types
import Zippy.Base.Common
import Zippy.Base.Data
import Zippy.Riak.Content (Index(..), Link(..))

group :: Proxy Group
group = Proxy

user :: Proxy User
user = Proxy

ownerIx :: Key User -> Index
ownerIx = keyIndex "owner"

creatorIx :: Key User -> Index
creatorIx = keyIndex "creator"

assignedToIx :: Key User -> Index
assignedToIx = keyIndex "assigned_to"

ownerLink :: Key User -> Link
ownerLink = link "owner"

memberLink :: Key User -> Link
memberLink = link "member"
