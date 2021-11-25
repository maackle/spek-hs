{-# LANGUAGE QuasiQuotes #-}

import qualified CMarkGFM                       as MD
import qualified Data.Text                      as T
import           Debug.Trace
import           Test.Framework
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit
import           Text.RawString.QQ

import qualified Specchio.Parse                 as P
import qualified Specchio.Token                 as K

toTokens :: T.Text -> [K.Token]
toTokens = P.toTokens . P.textToNodes

mod1 :: T.Text
mod1 = T.pack $ P.trim [r|
# module 1
|]

mod2 :: T.Text
mod2 = (T.pack "# module 2\n\n") <> mod2doc

mod2doc :: T.Text
mod2doc = T.pack $ P.trim [r|
Contains some documentation.

In multiple paragraphs.

  - and some
      - bullet points
  - at
      - varying
          - levels
|]

items1 :: T.Text
items1 = T.pack $ P.trim [r|
- [ ] first item
- [ ] second item
|]

-- test_roundtrip = (P.nodeToText $ MD.Node (Just $ MD.PosInfo 2 1 100 100) MD.DOCUMENT $ P.textToNodes mod2) @?= mod2
test_equivalence = (P.nodeToText $ MD.Node (Just $ MD.PosInfo 2 1 100 100) MD.DOCUMENT $ P.textToNodes mod2) @?= (P.nodeToText $ MD.Node Nothing MD.DOCUMENT $ P.textToNodes mod2 )

test_items = toTokens items1 @?= [K.mkTest "first item", K.mkTest "second item"]

test_mod1 = toTokens mod1 @?= [K.mkModule "module 1" Nothing]
test_mod2 = toTokens mod2 @?= [K.mkModule "module 2" $ Just mod2doc]

main :: IO ()
main = defaultMain
  [ testGroup "Main"
    [
      testCase "test item parsing 1" test_items
    , testCase "test module parsing 1" test_mod1
    , testCase "test module parsing 2" test_mod2
    ]
  ]
