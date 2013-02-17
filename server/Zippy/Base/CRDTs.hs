{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Zippy.Base.CRDTs where

class FromCRDT c a | c -> a where
    fromCRDT :: c -> a

data OpCounter a = HashMap

instance FromCRDT (OpCounter a) a where
    fromCRDT = 

data GCounter a

instance FromCRDT (GCounter a) a where
    fromCRDT 

data PNCounter a
instance FromCRDT (GCounter a) a where
    fromCRDT 

data NNCounter a
instance FromCRDT (GCounter a) a where
    fromCRDT 

data LWWRegister a
instance FromCRDT (GCounter a) a where
    fromCRDT 

data MVRegister a
instance FromCRDT (GCounter a) a where
    fromCRDT 

data GSet a
instance FromCRDT (GCounter a) a where
    fromCRDT 

data TPSet a
instance FromCRDT (GCounter a) a where
    fromCRDT 

data USet a
instance FromCRDT (GCounter a) a where
    fromCRDT 

data LWWSet a
instance FromCRDT (GCounter a) a where
    fromCRDT 

data PNSet a
instance FromCRDT (GCounter a) a where
    fromCRDT 

data MWSSet a
instance FromCRDT (GCounter a) a where
    fromCRDT 

data ORSet a
instance FromCRDT (GCounter a) a where
    fromCRDT 

--data TPTPGraph
--data AOMDAG
--data ARPartialOrder
--data RGArray
--data ContSeq



-- state based when allow_mult is true

-- op based requires that all updates are delivered at every 
-- replica in the specified delivery order, but can be emulated
-- via state, see Spec 4 in CRDT paper

