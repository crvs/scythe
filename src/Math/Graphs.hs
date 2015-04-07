module Math.Graphs where

import Control.Parallel(par,pseq)
import Data.List((\\),union)

data Vertex a = Vert a
    deriving Eq

instance Show a => Show (Vertex a) where
    show (Vert a) = show a

data Edge a = Edge (Vertex a) (Vertex a)
    deriving Eq

instance Show a => Show (Edge a) where
    show (Edge v1 v2) = "(" ++ show v1 ++ " -> " ++ show v2 ++ ")"

data Graph a = Graph { vertices :: [Vertex a] , edges :: [Edge a]}

instance Show a => Show (Graph a) where
    show (Graph vs es) = show vs ++ "\n" ++ show es

newGraph :: Graph a
newGraph = Graph [] []

addVertex :: Eq a => Graph a -> Vertex a -> Graph a
addVertex (Graph vs es) v = if v `elem` vs
    then Graph vs es -- we fail silently
    else Graph (v:vs) es

addEdge :: Eq a => Graph a -> Edge a -> Graph a
addEdge (Graph vs es) e@(Edge v1 v2)
    | e `elem` es = Graph vs es
    | otherwise     = addVertex (addVertex (Graph vs (e:es)) v2) v1

pureEdge :: (a,a) -> Edge a
pureEdge (x,y) = Edge (Vert x) (Vert y)

edge :: (Vertex a,Vertex a) -> Edge a
edge (x,y) = Edge x y

vertex :: a -> Vertex a
vertex = Vert

remVertex :: Eq a => Graph a -> Vertex a -> Graph a
remVertex (Graph vs es) v = Graph (vs \\ [v]) (filter (\x -> from x /= v && to x /= v) es)

from :: Edge a -> Vertex a
from (Edge v _) = v

to :: Edge a -> Vertex a
to (Edge _ v) = v

incoming :: Eq a => Graph a -> Vertex a -> [ Edge a ]
incoming (Graph _ es) v = filter ((==v) . to) es

incomingV :: Eq a => Graph a -> Vertex a -> [ Vertex a ]
incomingV (Graph _ es) v = map from $ filter ((==v) . to) es

outgoing :: Eq a => Graph a -> Vertex a -> [ Edge a ]
outgoing (Graph _ es) v = filter ((==v) . from) es

outgoingV :: Eq a => Graph a -> Vertex a -> [ Vertex a ]
outgoingV (Graph _ es) v = map to $ filter ((==v) . from) es

----------------------------------------------------------------


scytheDG :: Eq a => Graph a -- diag - diagram for which we want to compute the morse data
                  -> Graph a -- morse data of diag
scytheDG diag = scytheDGAux diag []

scytheDGAux :: Eq a => Graph a -- d  - diagram we are applying scythe to
                     -> [Vertex a]   -- cs - list of critical elements
                     -> Graph a -- the morse data of d after applying scythe with cs as critical elements
scytheDGAux d cs | length cs == (length . vertices) d = d -- this is true exactly when there are no non-critical elements left
                 | otherwise = let { mc = minNCritDG d cs -- find a minimal critical element
                     } in
                         scytheDGAux (
                             quePlayDG
                                 d
                                 [mc] -- the queue always starts with the new minimal critical element
                                 (mc:cs) -- we mark mc, the new critical, element as critical
                                 [mc] -- we also mark it so that it can not be requeued in this recursion
                                    )
                             (mc:cs)

quePlayDG :: Eq a => Graph a -- d    - the diagram we want to reduce
                   -> [Vertex a]   -- y:ys - the queue of elements of the diagram
                   -> [Vertex a]   -- c    - the list of critical elments
                   -> [Vertex a]   -- m    - the list of elements that were already enqueued
                   -> Graph a -- the resulting reduced diagram
quePlayDG d [] _ _ = d -- if the queue is empty then there is nothing left to do and we return the diagram
quePlayDG d (y:ys) cs ms =
    if length (nonCUnderDG y cs d) == 1 then -- there is a non-critical element over exactly one other non-critical
        quePlayDG
            (reducepairDG d x y) -- reduce the diagram d with respect to s and y
            nys -- y gets dequeued and we equeue all elements over x and y
            cs  -- the critical set is not changed by this operation
            nms -- add all new enqueued elements to the marked list
    else
        quePlayDG
            d   -- the diagram is unchanged at this stage
            nys -- enqueue every element over y that hasn't been enqueued yet
            cs  -- the critical elements remain the same
            nms -- mark the enqueued elements
    where
        x   = head (nonCUnderDG y cs d)
        nys = ys ++ ((outgoingV d x ++ outgoingV d y) \\ ms)
        nms = ms `union` nys
        fnys = ys ++ (outgoingV d y \\ ms)
        fnms = ms `union` nys

nonCUnderDG :: Eq a => Vertex a           -- y  - element in the diagram
                     -> [Vertex a]      -- cs - list of critical elements in d
                     -> Graph a -- d  - diagram
                     -> [Vertex a] -- list of the non-critical elments under y
nonCUnderDG y cs d =
        filter -- we filter out from the elements covered by y, those that have been marked as critical
            (`elem` (incomingV d y \\ cs))
            (vertices d)

minNCritDG :: Eq a => Graph a -- d  - diagram
                    -> [Vertex a]   -- cs - list of critical elments
                    -> Vertex a -- a minimal non critical elment
minNCritDG d cs =
    head $ filter -- we catch out those elments that have non-critical elments under them,
            (\ y -> null $ nonCUnderDG y cs d) -- and take the first one of those
            (vertices d)

reducepairDG :: Eq a => Graph a -- dgr - diagram tha we are reducing
                      -> Vertex a     -- x   - covered element
                      -> Vertex a     -- y   - covering element
                      -> Graph a -- dgr with the covering relation x<y removed
reducepairDG dgr x y = Graph
    (vertices dgr \\ [x,y])
    (filter
        (\ e -> not(to e `elem` [x,y] || from e `elem` [x,y]))
        (edges dgr ++ [ Edge w z | w <- incomingV dgr y , z <- outgoingV dgr x]))
