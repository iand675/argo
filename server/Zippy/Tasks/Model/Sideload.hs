{-# LANGUAGE TemplateHaskell #-}
module Zippy.Tasks.Model.Sideload where
import Zippy.Base.Model

data Sideload a = Reference { id :: Text }
				| Loaded { value :: a }

deriveJSON id' ''Sideload