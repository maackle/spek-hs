{-# LANGUAGE QuasiQuotes #-}

import           Test.Framework
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit
import           Text.RawString.QQ

t1 = 1 @?= 2

items1 :: String
items1 = [r|
- [ ] item1
- [ ] item2
|]

rust1 = undefined

main :: IO ()
main =
  defaultMain [testGroup "Main" [testCase "test" t1, testCase "undef" rust1]]
