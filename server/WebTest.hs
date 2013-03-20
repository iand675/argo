module Main where
import Test.Hspec
import Zippy.GroupTests
import Zippy.ListTests
import Zippy.TaskTests
import Zippy.Tests

main :: IO ()
main = hspec $ do
	groupTests
	listTests
	taskTests
	userTests