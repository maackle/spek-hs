module Specchio.Parse (textToNodes, toTokens) where

import qualified CMarkGFM        as MD
import           Data.List       (stripPrefix)
import qualified Data.Text       as T
import qualified Data.Vector     as V
import           Debug.Trace     (trace, traceShow, traceShowId)
import           Safe            (fromJustNote)
import           Specchio.Common (unimplemented)
import           Specchio.Spek   (Spek, SpekItem, SpekModule, addModule,
                                  emptySpek, makeModule)
import           Specchio.Token

data State = State [Token] [MD.Node]

-- addToken (State ts ns) t = (State (t : ts) ns)
addNode n (State ts ns) = State ts (n : ns)

textToNodes :: T.Text -> [MD.Node]
textToNodes md = children
  where
    MD.Node pos ty children = MD.commonmarkToNode [] [] md

nodeToText :: MD.Node -> T.Text
nodeToText = MD.nodeToCommonmark [] Nothing

nodesToText :: [MD.Node] -> T.Text
nodesToText ns = T.intercalate (T.pack "\n") (nodeToText <$> ns)

-- Render the current node buffer as a new token, clearing the node buffer
flush :: State -> State
flush (State ts []) = State ts []
flush (State ts ns) =
  State (toToken (reverse ns) : ts) []


visit :: State -> MD.Node -> State
visit state@(State toks nodes) node@(MD.Node pos ty children) =
  case ty of
    MD.HEADING 1  -> addNode node $ flush state
    MD.HEADING h  -> addNode node state
    MD.PARAGRAPH  -> addNode node state
    MD.LIST attrs -> foldl visit state children
    MD.ITEM       -> addNode node $ flush state
    n             -> trace ("unexpected node type" <> show n) state

toToken :: [MD.Node] -> Token
toToken [] = error "can't make a token out of empty node list"
toToken (n@(MD.Node pos ty children) : ns) = case ty of
  -- module
  MD.HEADING 1 -> mkModule (T.unpack $ nodeToText n) childText

  -- test
  MD.ITEM      ->
    case T.stripPrefix (T.pack "\\[ \\] ") childText of
      Just t  -> mkTest $ trim $ T.unpack t
      Nothing -> error "unimplemented"

  _            -> error "unimplemented"

  where
    childText = nodesToText children

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== '\n')

toTokens :: [MD.Node] -> [Token]
toTokens nodes =
  let
    State ts ns = flush $ foldl visit (State [] []) nodes
  in
    if not (null ns) then
      error "EOF reached, but there was more to parse"
    else
      reverse ts
