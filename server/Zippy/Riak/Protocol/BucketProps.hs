{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Zippy.Riak.Protocol.BucketProps (BucketProps(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data BucketProps = BucketProps{nVal :: !(P'.Maybe P'.Word32), allowMult :: !(P'.Maybe P'.Bool)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable BucketProps where
  mergeAppend (BucketProps x'1 x'2) (BucketProps y'1 y'2) = BucketProps (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default BucketProps where
  defaultValue = BucketProps P'.defaultValue P'.defaultValue
 
instance P'.Wire BucketProps where
  wireSize ft' self'@(BucketProps x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 13 x'1 + P'.wireSizeOpt 1 8 x'2)
  wirePut ft' self'@(BucketProps x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 13 x'1
             P'.wirePutOpt 16 8 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{nVal = Prelude'.Just new'Field}) (P'.wireGet 13)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{allowMult = Prelude'.Just new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> BucketProps) BucketProps where
  getVal m' f' = f' m'
 
instance P'.GPB BucketProps
 
instance P'.ReflectDescriptor BucketProps where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Protocol.BucketProps\", haskellPrefix = [MName \"Database\",MName \"Riak\"], parentModule = [MName \"Protocol\"], baseName = MName \"BucketProps\"}, descFilePath = [\"Database\",\"Riak\",\"Protocol\",\"BucketProps.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.nVal\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"nVal\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Protocol.BucketProps.allowMult\", haskellPrefix' = [MName \"Database\",MName \"Riak\"], parentModule' = [MName \"Protocol\",MName \"BucketProps\"], baseName' = FName \"allowMult\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
