module Main where
import Test.Hspec
import GroupTests
import ListTests
import TaskTests
import UserTests

main :: IO ()
main = hspec $ do
	groupTests
	listTests
	taskTests
	userTests