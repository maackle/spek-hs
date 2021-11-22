{-# LANGUAGE TemplateHaskell #-}

module Specchio.Spek
  ( Spek
  , SpekModule
  , SpekItem
  , emptySpek
  , addModule
  , makeModule
  ) where

import           Control.Lens
import qualified Data.Text        as T
import           Data.Vector      as V (Vector, empty, map, snoc)
import           Data.Vector.Lens

newtype Spek = Spek { _modules :: Vector SpekModule }

data SpekModule = SpekModule
  { _filename :: String
  , _label    :: String
  , _doc      :: Maybe T.Text
  , _items    :: Vector SpekItem
  }

data SpekItem = Test { _name :: String, _subs :: [String] }
              | Doc String

$(makeLenses ''Spek)
$(makeLenses ''SpekModule)
$(makeLenses ''SpekItem)

emptySpek :: Spek
emptySpek = Spek { _modules = empty }

makeModule :: String -> Maybe T.Text -> SpekModule
makeModule label doc =
  SpekModule { _filename = "TODO", _label = label, _doc = doc, _items = mempty }

addModule :: Spek -> SpekModule -> Spek
addModule spek mod = over modules (`V.snoc` mod) spek

addItem :: Spek -> SpekItem -> Spek
addItem spek item = over (modules . sliced i 1)
                         (V.map (over items (`V.snoc` item)))
                         spek
  where i = length $ _modules spek
