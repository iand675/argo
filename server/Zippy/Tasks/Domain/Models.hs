module Zippy.Tasks.Domain.Models where
import Zippy.Base.Model
import import qualified Zippy.Tasks.Web.Models as Web

data Task
data List
data Group

instance Changeset Task Web.Task where
    apply cs u = undefined

instance Changeset List Web.List where
    apply cs u = undefined

instance Changeset Group Web.Group where
    apply cs u = undefined