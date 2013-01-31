module Zippy.User.Domain.Models where
import Zippy.Base.Model
import qualified Zippy.User.Web.Models as Web

instance Changeset User Web.User where
    apply cs u = undefined

data User = User