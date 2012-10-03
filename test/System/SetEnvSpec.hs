module System.SetEnvSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Property

import qualified Control.Exception as E
import           System.IO.Error
import           GHC.IO.Exception (IOErrorType (InvalidArgument))
import           System.SetEnv
import           System.Environment (getEnv)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "unsetEnv" $ do
    it "removes specified environment variable" $ do
      setEnv "FOO" "foo"
      unsetEnv "FOO"
      getEnv "FOO" `shouldThrow` isDoesNotExistError

    it "does nothing if specified environment variable is not set" $ do
      unsetEnv "BAR"
      unsetEnv "BAR"
      getEnv "BAR" `shouldThrow` isDoesNotExistError

    it "throws an exception if key is the empty string" $ do
      unsetEnv "" `shouldThrow` (== InvalidArgument) . ioeGetErrorType

    it "throws an exception if key contains '='" $ do
      unsetEnv "some=key" `shouldThrow` (== InvalidArgument) . ioeGetErrorType

    it "works for arbitrary keys" $ property $ \k -> ('\NUL' `notElem` k && '=' `notElem` k && (not . null) k) ==> morallyDubiousIOProperty $ do
      setEnv k "foo"
      unsetEnv k
      (getEnv k >> return False) `E.catch` (return . isDoesNotExistError)

  describe "setEnv" $ do
    it "sets specified environment variable to given value" $ do
      unsetEnv "FOO"
      setEnv "FOO" "foo"
      getEnv "FOO" `shouldReturn` "foo"

    it "resets specified environment variable, if it is already set" $ do
      unsetEnv "FOO"
      setEnv "FOO" "foo"
      setEnv "FOO" "bar"
      getEnv "FOO" `shouldReturn` "bar"

    it "removes specified environment variable when value is the empty string" $ do
      setEnv "FOO" "foo"
      setEnv "FOO" ""
      getEnv "FOO" `shouldThrow` isDoesNotExistError

    it "removes specified environment variable when first character of value is NUL" $ do
      setEnv "FOO" "foo"
      setEnv "FOO" "\NULfoo"
      getEnv "FOO" `shouldThrow` isDoesNotExistError

    it "truncates value at NUL character" $ do
      unsetEnv "FOO"
      setEnv "FOO" "foo\NULbar"
      getEnv "FOO" `shouldReturn` "foo"

    it "truncates key at NUL character" $ do
      unsetEnv "FOO"
      setEnv "FOO\NULBAR" "foo"
      getEnv "FOO" `shouldReturn` "foo"

    it "works for unicode" $ do
      unsetEnv "FOO"
      setEnv "FOO" "foo-\955-bar"
      getEnv "FOO" `shouldReturn` "foo-\955-bar"

    it "works for arbitrary values" $ property $ \v -> ('\NUL' `notElem` v && (not . null) v) ==> morallyDubiousIOProperty $ do
      setEnv "FOO" v
      r <- getEnv "FOO"
      return (r == v)

    it "works for unicode keys" $ do
      setEnv "foo-\955-bar" "foo"
      getEnv "foo-\955-bar" `shouldReturn` "foo"

    it "throws an exception if key is the empty string" $ do
      setEnv "" "foo" `shouldThrow` (== InvalidArgument) . ioeGetErrorType

    it "throws an exception if key contains '='" $ do
      setEnv "some=key" "foo" `shouldThrow` (== InvalidArgument) . ioeGetErrorType

    it "works for arbitrary keys" $ property $ \k -> ('\NUL' `notElem` k && '=' `notElem` k && (not . null) k) ==> morallyDubiousIOProperty $ do
      setEnv k "foo"
      r <- getEnv k
      return (r == "foo")
