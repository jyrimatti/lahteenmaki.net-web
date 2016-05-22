{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, DataKinds, TypeSynonymInstances, FlexibleInstances #-}
module Util.Markup where

import Data.String (IsString(..))
import Control.Monad.Writer
import Data.List (groupBy)
import qualified Data.Text.Lazy as DT

data Empty = Empty
data Text = Text String
data RawMarkup = RawMarkup String
data Document = Document

instance (NodeLike a, LegalChild a String ~ Legal) => IsString (Writer [ChildOf a] b) where
  fromString str = writer (undefined, [Child (TextNode str)])

type ContentType = String
type Markup = (ContentType, Writer [ChildOf Document] Document)

instance NodeLike Empty where
    val _ = Empty
    name = undefined
instance NodeLike Text where
    val _ = Text ""
    name = undefined
instance NodeLike RawMarkup where
    val _ = RawMarkup ""
    name = undefined
instance NodeLike Document where
    val _ = Document
    name _ = undefined

empty :: (MonadWriter [ChildOf a] m, NodeLike a) => m Empty
empty = createNode Empty EmptyNode

text :: (MonadWriter [ChildOf a] m, NodeLike a, LegalChild a String ~ Legal) => String -> m Text
text str = createNode (Text str) $ TextNode str

raw :: (MonadWriter [ChildOf a] m, NodeLike a) => String -> m RawMarkup
raw str = createNode (RawMarkup str) $ RawMarkupNode str

data Attribute = Attribute String String
instance Show Attribute where
    show (Attribute key value) 
        | null key  = ""
        | otherwise = key ++ "=" ++ "\"" ++ escapeAttributeContent value ++ "\""

data Node = Node String Bool [Attribute] [Node]
          | TextNode String
          | RawMarkupNode String
          | EmptyNode

instance Show Node where
    show (Node n True attrs _) = "<" ++ n ++ showAttributes attrs ++ " />"
    show (Node n False attrs children) = "<" ++ n ++ showAttributes attrs ++ ">" ++ concatMap show children ++ "</" ++ n ++ ">"
    show (TextNode str) = escapeElementContent str
    show (RawMarkupNode str) = str
    show EmptyNode = ""

showAttributes :: [Attribute] -> String
showAttributes = (foldl joinWithSpace "") . (map show) . (concatMap joinValues) . (groupBy equalKey)
    where equalKey (Attribute k1 _) (Attribute k2 _) = k1 == k2
          value (Attribute _ v) = v
          joinWithSpace v1 v2 = v1 ++ " " ++ v2
          joinValues [] = []
          joinValues (Attribute k v1 : xs) = [Attribute k $ foldl joinWithSpace v1 $ map value xs]

render :: Markup -> DT.Text
render (_,markup) = DT.pack $ concatMap show $ map (\(Child n) -> n) $ execWriter markup

escapeAttributeContent :: String -> String
escapeAttributeContent = concatMap f
  where f '\'' = "&#39;"
        f '"'  = "&quot;"
        f '&'  = "&amp;"
        f '<'  = "&lt;"
        f '>'  = "&gt;"
        f c    = [c]
        
escapeElementContent :: String -> String
escapeElementContent = concatMap f
  where f '&'  = "&amp;"
        f '<'  = "&lt;"
        f '>'  = "&gt;"
        f c    = [c]

class AttributeLike a where
    toAttr :: a -> Attribute

class NodeLike a where
    val :: [Attribute] -> a
    name :: a -> String
    
    isVoid :: a -> Bool
    isVoid _ = False
    
    attr :: a -> [Attribute]
    attr _ = []
    
    toNode :: a -> [Node] -> Node
    toNode a = Node (name a) (isVoid a) (attr a)

data Legality = Legal

type family LegalAttribute a b :: Legality
type family LegalChild a b :: Legality

newtype ChildOf n = Child {child :: Node} 

(?) :: (NodeLike parent, MonadWriter [ChildOf parent] m, NodeLike child, LegalChild parent child ~ Legal) => ([Attribute] -> child) -> Writer [ChildOf child] a1 -> m parent
f ? b = createNode (val []) $ toNode (f []) $ map child $ execWriter b

createNode :: (NodeLike a, NodeLike b) => MonadWriter [ChildOf a] m => b -> Node -> m b
createNode parent childNode = writer (parent, [Child childNode])

(<<) :: (NodeLike a, AttributeLike b, LegalAttribute a b ~ Legal) => ([Attribute] -> a) -> b -> ([Attribute] -> a)
f << b = \aa -> f (toAttr b : aa)
