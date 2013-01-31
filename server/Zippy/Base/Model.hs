{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Zippy.Base.Model (
	module Data.Aeson,
	module Data.Aeson.TH,
	(<>),
	Text,
	ErrorResponse(..),
	stripPrefix,
	Changeset(..),
	Key,
	jsonize
) where
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Text (Text)
import Data.Monoid

type Key a = Text

stripPrefix i name = if i == 0
	then downcase rest
	else stripPrefix (i - 1) $ downcase rest
	where
		(_, rest) = break isUpper name
		downcase [] = []
		downcase (x:xs) = toLower x : xs

jsonize i n = deriveJSON (stripPrefix i) n  

class Changeset a b | a -> b where
    apply :: a -> b -> Either Text a

data ErrorResponse = UserError Text
				   | DataError Text
				   | WebError Text
				   | OtherError Text