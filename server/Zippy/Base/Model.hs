module Zippy.Base.Model (
	module Data.Aeson,
	module Data.Aeson.TH,
	(<>),
	Text,
	id',
	ErrorResponse(..)
) where
import Data.Aeson
import Data.Aeson.TH
import Data.Text
import Data.Monoid

id' :: a -> a
id' = id

data ErrorResponse = ErrorResponse