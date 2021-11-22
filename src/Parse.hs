module Parse () where

import qualified CMarkGFM    as MD
import qualified Data.Text   as T
import qualified Data.Vector as V
import           Safe        (fromJustNote)
import           Spek        (Spek, SpekItem, SpekModule, addModule, emptySpek,
                              makeModule)

todo :: [Char] -> a
todo reason = error $ "TODO: " <> reason

unimplemented :: a
unimplemented = error "unimplemented"

data Buffer = Buffer BufferType (V.Vector MD.Node)

data BufferType
  = Module
  | Test
  | Doc
  | Unknown

data State = State Spek (Maybe Buffer)

isContainer :: MD.NodeType -> Bool
isContainer ty =
  case ty of
    MD.PARAGRAPH -> True
    MD.LIST _    -> True
    _            -> False

flush :: Spek -> Buffer -> Spek
flush spek (Buffer ty buf) =
  case ty of
    Module ->
      let label = T.unpack $ MD.nodeToCommonmark [] Nothing top
          doc = nodesToDoc rest
          mod = makeModule label doc
       in addModule spek mod
    _ -> unimplemented
  where
    -- Test ->

    (top, rest) = fromJustNote "Can't flush an empty buffer" $ V.uncons buf
    MD.Node _ topType _ = top

nodesToDoc :: V.Vector MD.Node -> Maybe T.Text
nodesToDoc nodes = Just $ T.intercalate (T.pack " ") $ V.toList (MD.nodeToCommonmark [] Nothing <$> nodes)

-- visit :: State -> MD.Node -> State
-- visit state@(State spek buf) node =
--   if isContainer ty
--     then foldl visit state childreng <$> nodes)

-- visit :: State -> MD.Node -> State
-- visit state@(State spek buf) node =
--   if isContainer ty
--     then foldl visit state children
--     else visit' state node
--   where
--     MD.Node pos ty children = node

-- visit' :: State -> MD.Node -> State
-- visit' state@(State spek buf) node =
--   case buf of
--     Just (Module name ps) ->
--       case ty of
--         MD.PARAGRAPH -> State spek $ Just (Module name (ps ++ [node]))
--         _ -> error "unexpected"
--     _ -> unimplemented
--   where
--     MD.Node pos ty children = node

-- case (MD.type, buf) of
--   (MD.HEADING level, )

fromMarkdown :: T.Text -> Spek
fromMarkdown md = spek
  where
    MD.Node pos ty children = MD.commonmarkToNode [] [] md
    spek = undefined
