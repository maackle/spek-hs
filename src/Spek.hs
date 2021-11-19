{-# LANGUAGE TemplateHaskell #-}

module Spek (Spek, SpekModule, SpekItem, emptySpek) where

import Control.Lens
import Data.Vector as V (Vector, empty, snoc)
import Data.Vector.Lens

newtype Spek = Spek {_modules :: Vector SpekModule}

data SpekModule = SpekModule
  { _filename :: String,
    _label :: String,
    _doc :: Maybe String,
    _items :: Vector SpekItem
  }

data SpekItem
  = Test {_name :: String, _subs :: [String]}
  | Doc String

$(makeLenses ''Spek)
$(makeLenses ''SpekModule)
$(makeLenses ''SpekItem)

emptySpek :: Spek
emptySpek = Spek {_modules = empty}

addModule :: Spek -> SpekModule -> Spek
addModule spek mod = over modules (`V.snoc` mod) spek

addItem :: Spek -> SpekItem -> Spek
addItem spek item = over (modules) (over items (`V.snoc` item) . last) spek
