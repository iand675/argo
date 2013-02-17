{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Zippy.Riak.Protocol.SearchQueryResponse (SearchQueryResponse(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Zippy.Riak.Protocol.SearchDocument as Protocol (SearchDocument)
 
data SearchQueryResponse = SearchQueryResponse{documents :: !(P'.Seq Protocol.SearchDocument), maxScore :: !(P'.Maybe P'.Float),
                                               numFound :: !(P'.Maybe P'.Word32)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable SearchQueryResponse where
  mergeAppend (SearchQueryResponse x'1 x'2 x'3) (SearchQueryResponse y'1 y'2 y'3)
   = SearchQueryResponse (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default SearchQueryResponse where
  defaultValue = SearchQueryResponse P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire SearchQueryResponse where
  wireSize ft' self'@(SearchQueryResponse x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeOpt 1 2 x'2 + P'.wireSizeOpt 1 13 x'3)
  wirePut ft' self'@(SearchQueryResponse x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
             P'.wirePutOpt 21 2 x'2
             P'.wirePutOpt 24 13 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{documents = P'.append (documents old'Self) new'Field}) (P'.wireGet 11)
             21 -> Prelude'.fmap (\ !new'Field -> old'Self{maxScore = Prelude'.Just new'Field}) (P'.wireGet 2)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{numFound = Prelude'.Just new'Field}) (P'.wireGet 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> SearchQueryResponse) SearchQueryResponse where
  getVal m' f' = f' m'
 
instance P'.GPB SearchQueryResponse
 
instance P'.ReflectDescriptor SearchQueryResponse where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 21, 24])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.SearchQueryResponse\", haskellPrefix = [MName \"Database\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"SearchQueryResponse\"}, descFilePath = [\"Database\",\"Riak\",\"Protocol\",\"SearchQueryResponse.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryResponse.Documents\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryResponse\"], baseName' = FName \"documents\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Protocol.SearchDocument\", haskellPrefix = [MName \"Database\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"SearchDocument\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryResponse.maxScore\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryResponse\"], baseName' = FName \"maxScore\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 21}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.SearchQueryResponse.numFound\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"SearchQueryResponse\"], baseName' = FName \"numFound\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
