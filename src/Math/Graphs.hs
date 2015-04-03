module Math.Graphs where

-----------------------------------------------------------------
--{{{ primitive data types
-----------------------------------------------------------------

data Vertex a = Vert a
    deriving Eq

instance Show a => Show (Vertex a) where
    show (Vert a) = show a

data Edge a = Edge (Vertex a) (Vertex a)
    deriving Eq

instance Show a => Show (Edge a) where
    show (Edge v1 v2) = "(" ++ show v1 ++ " -> " ++ show v2 ++ ")"
    -- parse edges

data Graph a = Graph [ Vertex a ] [ Edge a ]

instance Show a => Show (Graph a) where
    show (Graph vs es) = show vs ++ "\n" ++ show es

--}}}------------------------------------------------------------
--{{{ primitive constructors
-----------------------------------------------------------------

newGraph :: Graph a
newGraph = Graph [] []

addVertex :: Eq a => Graph a -> Vertex a -> Graph a
addVertex (Graph vs es) v = if v `elem` vs
    then Graph vs es
    else Graph (v:vs) es

addEdge :: Eq a => Graph a -> Edge a -> Graph a
addEdge (Graph vs es) e @(Edge v1 v2) =
    if (e `elem` es) || not ((v1 `elem` vs) && (v2 `elem` vs))
    then Graph vs es
    else Graph vs (e:es)

--}}}------------------------------------------------------------
--{{{ primitive data rendering
-----------------------------------------------------------------

vertices :: Graph a -> [Vertex a]
vertices (Graph vs _) = vs

edges :: Graph a -> [Edge a]
edges (Graph _ es) = es

from :: Edge a -> Vertex a
from (Edge v _) = v

to :: Edge a -> Vertex a
to (Edge _ v) = v

incoming :: Eq a => Graph a -> Vertex a -> [ Edge a ]
incoming (Graph _ es) v = filter (\ (Edge _ v') -> v' == v) es

outgoing :: Eq a => Graph a -> Vertex a -> [ Edge a ]
outgoing (Graph _ es) v = filter (\ (Edge v' _) -> v' == v) es
