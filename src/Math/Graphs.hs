module Math.Graphs where

import Control.Monad
import Control.Applicative
import Data.List(union,(\\))
-- import qualified Data.ByteString.Char8 as B

-------------------------
-- general purpose graphs

-- no notion of vertex is necessary for general purpose graphs

data Edge a = Edge { from :: a , to :: a }
    deriving Eq

instance Show a => Show (Edge a) where
    show (Edge v1 v2) = "(" ++ show v1 ++ "--" ++ show v2 ++ ")"

data Graph a = Graph { vertices :: [a] , edges :: [Edge a]}

instance Show a => Show (Graph a) where
    show (Graph vs es) = show vs ++ "\n" ++ show es

--------------------------
-- elements marked with a boolean

data Marked a = Mark { label :: a , mark :: Bool }

nMark :: Marked a -> Bool
nMark = not.mark

markT,markF :: Marked a -> Marked a
markT (Mark a _) = Mark a True
markF (Mark a _) = Mark a False

tMark :: Marked a -> Marked a
tMark (Mark l b) = Mark l (not b)

instance Monad Marked where
    (>>=) a f = Mark ((label.f.label) a) (mark a && (mark.f.label) a)
    return a = Mark a False

instance Functor Marked where
    fmap g (Mark a b) = Mark (g a) b

instance Applicative Marked where
    pure = return
    (<*>) = ap

instance Eq a => Eq (Marked a) where
    v == v' = label v == label v'

instance Show a => Show (Marked a) where
    show v = show (label v) ++ (if mark v then "*" else [])
--------------------------
newGraph :: Graph a
newGraph = Graph [] []

incoming, outgoing :: Eq a => Graph a -> a -> [a]
incoming g v = map from (filter ((==v) . to)   (edges g))
outgoing g v = map to   (filter ((==v) . from) (edges g))

addEdge :: Eq a => Graph a -> Edge a -> Graph a
addEdge g e = Graph (vertices g `union` [from e,to e]) (edges g `union` [e])

addVertex :: Eq a => Graph a -> a -> Graph a
addVertex (Graph vs es) v = if v `elem` vs
    then Graph vs es -- we fail silently
    else Graph (v:vs) es

edge :: (a,a) -> Edge a
edge (x,y) = Edge x y

remVertex :: Eq a => Graph a -> a -> Graph a
remVertex (Graph vs es) v = Graph (vs \\ [v]) (filter (\x -> from x /= v && to x /= v) es)

graphUnion :: Eq a => Graph a -> Graph a -> Graph a
graphUnion ga gb = Graph (vertices ga `union` vertices gb) (edges ga `union` edges gb)

uniteGraphs :: Eq a => [Graph a] -> Graph a
uniteGraphs = foldr graphUnion newGraph

--------------------------
-- for graphs with marked vertices

mUnder, umUnder :: Eq a => Graph (Marked a) -> Marked a -> [Marked a]
mUnder   g v = filter mark  (incoming g v)
umUnder  g v = filter nMark (incoming g v)

mOver, umOver :: Eq a => Graph (Marked a) -> Marked a -> [Marked a]
mOver    g v = filter mark  (outgoing g v)
umOver   g v = filter nMark (outgoing g v)
