{-# LANGUAGE GADTs, ScopedTypeVariables, Rank2Types, OverloadedStrings #-}
module Zippy.Riak.MapReduce where
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import Data.Set (Set, toList)
import Data.Tagged
import Data.Text (Text)
import Data.Vector (fromList)
import Zippy.Riak.Types

type UntypedKey = ByteString

data Function = Erlang ErlangQuery
              | Javascript JavascriptQuery

data ErlangQuery = ErlangQuery { erlangModule :: ByteString
                               , function :: ByteString }

data JavascriptQuery = SourceQuery ByteString
                     | forall a. Stored (Bucket a) (Key a)
                     | BuiltIn ByteString

data Inputs a = Index { ixBucket :: Maybe (Bucket a)
                      , index    :: Maybe (IndexName a)
                      , ixStart  :: Maybe ByteString
                      , ixEnd    :: Maybe ByteString
                      , ixKey    :: Maybe (Key a)
                      }
            | KeyFilter { keyFilterBucket :: Bucket a
                        , keyFilter :: KeyFilter
                        }
            | Inputs { inputs :: [(Bucket a, Key a, Maybe ByteString)] }

instance ToJSON (Inputs a) where
	toJSON (Index bucket index start end key) = object [ "bucket" .= fmap unTagged bucket, "index" .= index, "key" .= fmap unTagged key, "start" .= start, "end" .= end ]
	toJSON (KeyFilter bucket filter) = object [ "bucket" .= unTagged bucket, "key_filters" .= filter ]
	toJSON (Inputs inputs) = toJSON $ map detuple inputs
		where
			detuple (b, k, Nothing) = [toJSON $ unTagged b, toJSON $ unTagged k]
			detuple (b, k, Just d)  = [toJSON $ unTagged b, toJSON $ unTagged k, toJSON d]

simpleInputs :: [(ByteString, ByteString)] -> Inputs ix
simpleInputs = Inputs . map toInput
	where toInput (b, k) = (Tagged b, Tagged k, Nothing)

sourceType :: Function -> Value
sourceType (Erlang _) = String "erlang"
sourceType (Javascript _) = String "javascript"

data MapReduce ix = MapReduce { mrInputs :: Inputs ix
                              , mrQuery :: [Phase]
                              }

instance ToJSON (MapReduce ix) where
	toJSON m = object [ "inputs" .= mrInputs m, "query" .= mrQuery m ]

data Phase = Map { mapSource :: Function
                 , mapArg    :: ByteString
                 , keep      :: Maybe Bool
                 }
           | Reduce { reduceSource :: Function
                    , reduceArg    :: ByteString
                    , keep         :: Maybe Bool
                    }
           | Link { bucket   :: Maybe ByteString
                  , tag      :: Maybe ByteString
                  , keep     :: Maybe Bool
                  }

shouldKeep :: Maybe Bool -> [Pair]
shouldKeep Nothing = []
shouldKeep (Just b) = [ "keep" .= b ]

sourcePairs :: Function -> [Pair]
sourcePairs (Erlang f) = case f of
  ErlangQuery m f -> [ "module" .= m, "function" .= f ]
sourcePairs (Javascript f) = case f of
  SourceQuery s -> [ "source" .= s ]
  Stored b k -> [ "bucket" .= unTagged b, "key" .= unTagged k ]
  BuiltIn n -> [ "name" .= n ]

instance ToJSON Phase where
	toJSON (Map src a k)    = object [ "map" .= object pairs ]
	  where
	    pairs = ["language" .= sourceType src] ++ sourcePairs src ++ shouldKeep k
	toJSON (Reduce src a k) = object $ [ "reduce" .= object pairs ]
	  where
	    pairs = ["language" .= sourceType src] ++ sourcePairs src ++ shouldKeep k
	toJSON (Link bkt tag k) = object $ [ "link" .= object [ "bucket" .= bkt, "tag" .= tag, "keep" .= k ]]

-- Key Filter logic

infixr 9 $>

data Transform from to where
	IntToText :: Transform Int Text
	TextToInt :: Transform Text Int
	FloatToText :: Transform Double Text
	TextToFloat :: Transform Text Double
	ToUpper :: Transform Text Text
	ToLower :: Transform Text Text
	Tokenize :: Text -> Int -> Transform Text Text
	UrlDecode :: Transform Text Text

newtype Filter a b = Filter { fromFilter :: [Value] }

finalizeFilter :: Filter Text a -> Filter Text ()
finalizeFilter = Filter . fromFilter

($>) :: (ToJSON a, ToJSON b, ToJSON c) => Transform a b -> Filter b c -> Filter a c
t $> (Filter ts) = Filter (toJSON t : ts)

predicate :: ToJSON a => Predicate a -> Filter a ()
predicate p = (Filter $ (:[]) $ toJSON p) :: Filter a b

data Predicate a where
	GreaterThan :: (ToJSON a, Num a) => a -> Predicate b
	LessThan :: (ToJSON b, Num b) => b -> Predicate b
	GreaterThanEq :: (ToJSON b, Num b) => b -> Predicate b
	LessThanEq :: (ToJSON b, Num b) => b -> Predicate b
	Between :: (ToJSON b, Num b) => b -> b -> Bool -> Predicate b
	Matches :: Text -> Predicate Text
	NotEqual :: (ToJSON b) => b -> Predicate b
	Equal :: (ToJSON b) => b -> Predicate b
	SetMember :: (ToJSON b) => Set b -> Predicate b
	SimilarTo :: Text -> Int -> Predicate Text
	StartsWith :: Text -> Predicate Text
	EndsWith :: Text -> Predicate Text
	And :: (ToJSON a) => Filter a () -> Filter a () -> Predicate a
	Or :: (ToJSON a) => Filter a () -> Filter a () -> Predicate a
	Not :: (ToJSON a) => Predicate a -> Predicate a

arr :: [Value] -> Value
arr = Array . fromList

type KeyFilter = Filter Text ()

instance ToJSON a => ToJSON (Predicate a) where
	toJSON (GreaterThan n)   = arr [String "less_than", toJSON n]
	toJSON (LessThan n)      = arr [String "less_than", toJSON n]
	toJSON (GreaterThanEq n) = arr [String "greater_than_eq", toJSON n]
	toJSON (LessThanEq n)    = arr [String "less_than_eq", toJSON n]
	toJSON (Between x y i)   = arr [String "between", toJSON x, toJSON y, toJSON i]
	toJSON (Matches str)     = arr [String "matches", toJSON str]
	toJSON (NotEqual x)      = arr [String "neq", toJSON x]
	toJSON (Equal x)         = arr [String "eq", toJSON x]
	toJSON (SetMember s)     = arr $ (String "set_member" :) $ map toJSON $ toList s
	toJSON (SimilarTo s d)   = arr [String "similar_to", toJSON s, toJSON d]
	toJSON (StartsWith str)  = arr [String "starts_with", toJSON str]
	toJSON (EndsWith str)    = arr [String "ends_with", toJSON str]
	toJSON (And l r)         = arr [String "and", toJSON l, toJSON r]
	toJSON (Or l r)          = arr [String "or", toJSON l, toJSON r]
	toJSON (Not t)           = arr [String "not", toJSON t]

instance (ToJSON a, ToJSON b) => ToJSON (Transform a b) where
	toJSON IntToText        = arr [String "int_to_string"]
	toJSON TextToInt        = arr [String "string_to_int"]
	toJSON FloatToText      = arr [String "float_to_string"]
	toJSON TextToFloat      = arr [String "string_to_float"]
	toJSON ToUpper          = arr [String "to_upper"]
	toJSON ToLower          = arr [String "to_lower"]
	toJSON (Tokenize str i) = arr [String "tokenize", toJSON str, toJSON i]
	toJSON UrlDecode        = arr [String "url_decode"]

instance (ToJSON b) => ToJSON (Filter a b) where
	toJSON = toJSON . fromFilter

