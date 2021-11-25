{-# LANGUAGE LambdaCase #-}

module Specchio.Parse (textToNodes, toTokens, nodeToText, trim, overText, traceL) where

import qualified CMarkGFM        as MD
import           Data.List       (stripPrefix)
import           Data.Maybe      (fromJust, fromMaybe, isJust)
import qualified Data.Text       as T
import qualified Data.Vector     as V
import           Debug.Trace     (trace, traceShowId)
import           Safe            (fromJustNote)
import           Specchio.Common (unimplemented)
import           Specchio.Spek   (Spek, SpekItem, SpekModule, addModule,
                                  emptySpek, makeModule)
import           Specchio.Token

data State = State [Token] [MD.Node] deriving (Show)

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True a  = Just a

traceL l v = trace ("\n||| " <> l <> ": " <> show v <> " ||| \n") v

addNode n (State ts ns) = State ts (n : ns)

textToNodes :: T.Text -> [MD.Node]
textToNodes md = children
  where
    MD.Node pos ty children = MD.commonmarkToNode [] [] md

nodeToText :: MD.Node -> T.Text
nodeToText = overText trim . MD.nodeToCommonmark [] Nothing

nodesToText :: [MD.Node] -> T.Text
nodesToText ns = nodeToText $ MD.Node Nothing MD.DOCUMENT ns

-- Render the current node buffer as a new token, clearing the node buffer
flush :: State -> State
flush (State ts []) = State ts []
flush (State ts ns) =
  let
    newToks = fromMaybe ts $ (:) <$> toToken (reverse ns) <*> Just ts
  in
    State newToks []


visit :: State -> MD.Node -> State
visit state@(State toks nodes) node@(MD.Node pos ty children) =
  case ty of
    MD.HEADING 1  -> addNode node $ flush state
    MD.HEADING h  -> addNode node state
    MD.PARAGRAPH  -> addNode node state
    MD.LIST attrs -> if allCheckboxes children
      then foldl visit state children
      else addNode node state

    -- the item must be a checkbox if it's being visited.
    -- if it weren't, it would be a child of a LIST.
    MD.ITEM       -> addNode node $ flush state
    MD.CODE_BLOCK _ t  -> addNode node state
    n             -> trace ("unexpected node type" <> show n) state

-- All nodes are checkboxes
-- TODO: error if there is a mixture
allCheckboxes :: [MD.Node] -> Bool
allCheckboxes = all check
  where
    check :: MD.Node -> Bool
    check (MD.Node _ ty children) = (ty == MD.ITEM) && isJust (checkboxText (nodesToText children))

toToken :: [MD.Node] -> Maybe Token
toToken [] = error "can't make a token out of empty node list"
toToken all@(n@(MD.Node pos ty children) : ns) = case ty of
  -- module
  MD.HEADING 1 -> Just $ mkModule (T.unpack $ fromJust $ headingText $ nodeToText n) $ toMaybe (not $ null ns) (nodesToText ns)

  -- test
  MD.ITEM      ->
    -- TODO: need a deeper check than this, because some documentation might start
    -- with a non-checkbox with ellipses
    case checkboxText (nodesToText children) of
      Just t  -> Just $ mkTest $ T.unpack t
      Nothing -> error "unimplemented"

  MD.LIST _ -> Just $ mkComment $ nodesToText all

  _            -> error "unimplemented"

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== '\n')

overText :: (String -> String) -> T.Text -> T.Text
overText f = T.pack . f . T.unpack

toTokens :: [MD.Node] -> [Token]
toTokens nodes =
  let
    State ts ns = flush $ foldl visit (State [] []) nodes
  in
    if not (null ns) then
      error "EOF reached, but there was more to parse"
    else
      reverse ts

checkboxText :: T.Text -> Maybe T.Text
checkboxText =  T.stripPrefix (T.pack "\\[ \\] ") . overText trim

headingText :: T.Text -> Maybe T.Text
headingText = T.stripPrefix (T.pack "# ")
