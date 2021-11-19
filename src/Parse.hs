module Parse () where

import qualified CMarkGFM as MD
import Data.Text (Text)
import Spek (Spek, SpekItem, SpekModule, emptySpek)

data Buffer
  = Module String [MD.Node]
  | Test [String]
  | Doc [String]

data State = State Spek (Maybe Buffer)

todo reason = error $ "TODO: " <> reason

unimplemented = error "unimplemented"

isContainer :: MD.NodeType -> Bool
isContainer ty =
  case ty of
    MD.PARAGRAPH -> True
    MD.LIST _ -> True
    _ -> False

flush :: State -> State
flush (State spek buf) =
  case buf of
    Module name docs ->
      Doc
        State

visit :: State -> MD.Node -> State
visit state@(State spek buf) node =
  if isContainer ty
    then foldl visit state children
    else visit' state node
  where
    MD.Node pos ty children = node

visit' :: State -> MD.Node -> State
visit' state@(State spek buf) node =
  case buf of
    Just (Module name ps) ->
      case ty of
        MD.PARAGRAPH -> State spek $ Just (Module name (ps ++ [node]))
        _ -> error "unexpected"
    _ -> unimplemented
  where
    MD.Node pos ty children = node

-- case (MD.type, buf) of
--   (MD.HEADING level, )

fromMarkdown :: Text -> Spek
fromMarkdown md = spek
  where
    MD.Node pos ty children = MD.commonmarkToNode [] [] md
    spek = undefined
