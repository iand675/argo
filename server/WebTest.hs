module Main where
import Test.Hspec
import Zippy.User.Web.GroupTests
import Zippy.User.Web.ListTests
import Zippy.User.Web.TaskTests
import Zippy.User.Web.Tests

main :: IO ()
main = hspec $ do
	groupTests
	listTests
	taskTests
	userTests