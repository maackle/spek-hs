module Specchio.Token (Token, mkModule, mkTest, mkComment) where

import qualified Data.Text as T


data Token
  = Module String (Maybe T.Text)
  | Test String
  | Comment T.Text
  deriving (Eq, Show)

mkModule = Module
mkTest = Test
mkComment = Comment
