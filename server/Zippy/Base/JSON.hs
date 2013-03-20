{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Zippy.Base.JSON (
	module Data.Aeson,
	module Data.Aeson.TH,
	(<>),
	Text,
	stripPrefix,
	jsonize
) where
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Text (Text)
import Data.Monoid
import Language.Haskell.TH

stripPrefix :: Int -> String -> String
stripPrefix i name = if i == 0
	then downcase rest
	else stripPrefix (i - 1) $ downcase rest
	where
		(_, rest) = break isUpper name
		downcase [] = []
		downcase (x:xs) = toLower x : xs

jsonize :: Int -> Name -> Q [Dec]
jsonize i = deriveJSON (stripPrefix i)
