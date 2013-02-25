{-# LANGUAGE OverloadedStrings #-}
module Zippy.User.Session where
import Zippy.Base.Common
import Zippy.Base.Web
import Zippy.User.Domain.Models

currentUserId :: Handler c (Key User)
currentUserId = return $ Key "iand675"