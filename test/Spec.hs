import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

t1 = 1 @?= 2

main :: IO ()
main =
  defaultMain
    [ testGroup
        "Main"
        [testCase "test" t1]
    ]