module Specchio.Common (todo, unimplemented) where

todo :: [Char] -> a
todo reason = error $ "TODO: " <> reason

unimplemented :: a
unimplemented = error "unimplemented"

