{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Zippy.Riak.Protocol.GetRequest (GetRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data GetRequest = GetRequest{bucket :: !P'.ByteString, key :: !P'.ByteString, r :: !(P'.Maybe P'.Word32),
                             pr :: !(P'.Maybe P'.Word32), basicQuorum :: !(P'.Maybe P'.Bool), notfoundOk :: !(P'.Maybe P'.Bool),
                             ifModified :: !(P'.Maybe P'.ByteString), head :: !(P'.Maybe P'.Bool),
                             deletedvclock :: !(P'.Maybe P'.Bool)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable GetRequest where
  mergeAppend (GetRequest x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9) (GetRequest y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9)
   = GetRequest (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
 
instance P'.Default GetRequest where
  defaultValue
   = GetRequest P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire GetRequest where
  wireSize ft' self'@(GetRequest x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 12 x'1 + P'.wireSizeReq 1 12 x'2 + P'.wireSizeOpt 1 13 x'3 + P'.wireSizeOpt 1 13 x'4 +
             P'.wireSizeOpt 1 8 x'5
             + P'.wireSizeOpt 1 8 x'6
             + P'.wireSizeOpt 1 12 x'7
             + P'.wireSizeOpt 1 8 x'8
             + P'.wireSizeOpt 1 8 x'9)
  wirePut ft' self'@(GetRequest x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 12 x'1
             P'.wirePutReq 18 12 x'2
             P'.wirePutOpt 24 13 x'3
             P'.wirePutOpt 32 13 x'4
             P'.wirePutOpt 40 8 x'5
             P'.wirePutOpt 48 8 x'6
             P'.wirePutOpt 58 12 x'7
             P'.wirePutOpt 64 8 x'8
             P'.wirePutOpt 72 8 x'9
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{bucket = new'Field}) (P'.wireGet 12)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{key = new'Field}) (P'.wireGet 12)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{r = Prelude'.Just new'Field}) (P'.wireGet 13)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{pr = Prelude'.Just new'Field}) (P'.wireGet 13)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{basicQuorum = Prelude'.Just new'Field}) (P'.wireGet 8)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{notfoundOk = Prelude'.Just new'Field}) (P'.wireGet 8)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{ifModified = Prelude'.Just new'Field}) (P'.wireGet 12)
             64 -> Prelude'.fmap (\ !new'Field -> old'Self{head = Prelude'.Just new'Field}) (P'.wireGet 8)
             72 -> Prelude'.fmap (\ !new'Field -> old'Self{deletedvclock = Prelude'.Just new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> GetRequest) GetRequest where
  getVal m' f' = f' m'
 
instance P'.GPB GetRequest
 
instance P'.ReflectDescriptor GetRequest where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [10, 18]) (P'.fromDistinctAscList [10, 18, 24, 32, 40, 48, 58, 64, 72])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.GetRequest\", haskellPrefix = [MName \"Database\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"GetRequest\"}, descFilePath = [\"Database\",\"Riak\",\"Protocol\",\"GetRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.GetRequest.bucket\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"GetRequest\"], baseName' = FName \"bucket\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.GetRequest.key\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"GetRequest\"], baseName' = FName \"key\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.GetRequest.r\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"GetRequest\"], baseName' = FName \"r\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.GetRequest.pr\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"GetRequest\"], baseName' = FName \"pr\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.GetRequest.basicQuorum\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"GetRequest\"], baseName' = FName \"basicQuorum\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.GetRequest.notfoundOk\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"GetRequest\"], baseName' = FName \"notfoundOk\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.GetRequest.ifModified\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"GetRequest\"], baseName' = FName \"ifModified\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.GetRequest.head\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"GetRequest\"], baseName' = FName \"head\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 64}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.GetRequest.deletedvclock\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"GetRequest\"], baseName' = FName \"deletedvclock\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
