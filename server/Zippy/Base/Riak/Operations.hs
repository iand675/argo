{-# LANGUAGE GADTs #-}
module Zippy.Base.Riak.Operations where

data Link = Link
	{ bucket :: Maybe ByteString
	, key :: Maybe ByteString
	, tag :: Maybe ByteString
	}

data Content a = Content
	{ value :: a
	, contentType :: Maybe ByteString
	, charset :: Maybe ByteString
	, vtag :: Maybe ByteString
	, links :: [Link]
	, lastModified :: Maybe Int
	, lastModifiedUSecs :: Maybe Int
	, userMetadata :: [(ByteString, ByteString)]
	, indexes :: [(ByteString, ByteString)]
	}

class RiakObject a where
	new :: a -> Content a
	combine :: a -> Content a -> a
	encode :: Content a -> Content ByteString
	decode :: Content ByteString -> Content (Either String a)

newContent ::
	a -> 
	Maybe ByteString ->
	Maybe ByteString ->
	[Link] ->
	[(ByteString, ByteString)] ->
	[(ByteString, ByteString)] ->
	Content a
newContent v ct cs ls um is = Content v ct cs Nothing ls Nothing Nothing um is

data Riak a where
	Get :: ByteString -> ByteString -> Riak (Maybe [Content ByteString])
	Put :: ByteString -> Maybe ByteString -> Content -> Riak (Maybe [Content ByteString])
	Delete :: ByteString -> ByteString -> Riak ()

--get :: 
--put
--delete
--mapReduce
--search
--index