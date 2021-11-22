{-# LANGUAGE QuasiQuotes #-}

import qualified CMarkGFM                       as MD
import qualified Data.Text                      as T
import           Test.Framework
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit
import           Text.RawString.QQ

import qualified Specchio.Parse                 as P
import qualified Specchio.Token                 as K

toTokens :: String -> [K.Token]
toTokens = P.toTokens . P.textToNodes . T.pack

mod1 :: String
mod1 = [r|
# module 1
|]

mod2 :: String
mod2 = [r|
# module 2

Contains some documentation.

In multiple paragraphs

- and some
  - bullet points

```rust
// and even some code
```
|]

items1 :: String
items1 = [r|
- [ ] first item
- [ ] second item
|]

test_items = toTokens items1 @?= [K.mkTest "first item", K.mkTest "second item"]

test_mod1 = toTokens mod1 @?= [K.mkModule "module 1" mempty]
test_mod2 = toTokens mod1 @?= [K.mkModule "module 2" (T.pack "TODO")]

main :: IO ()
main = defaultMain
  [ testGroup "Main"
    [ testCase "test item parsing 1" test_items
    , testCase "test module parsing 1" test_mod1
    , testCase "test module parsing 2" test_mod2
    ]
  ]
