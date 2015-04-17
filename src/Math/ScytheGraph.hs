module Math.ScytheGraph where

import Math.Graphs
import Data.List((\\),union)

type Vertex a = Marked (Marked a)
-- we mark the vertices twice

markCrit,unmarkCrit :: Eq a => Graph (Vertex a) -> Vertex a -> Graph (Vertex a)
markCrit   g v = fmap (\ x -> if x==v then fmap markT x else x) g
unmarkCrit g v = fmap (\ x -> if x==v then fmap markF x else x) g

markQued,unmarkQued :: Eq a => Graph (Vertex a) -> Vertex a -> Graph (Vertex a)
markQued g v   = fmap (\ x -> if x==v then markT x else x) g
unmarkQued g v = fmap (\ x -> if x==v then markF x else x) g

markQuedL,unmarkQuedL :: Eq a => Graph (Vertex a) -> [Vertex a] -> Graph (Vertex a)
markQuedL g v   = fmap (\ x -> if x `elem` v then markT x else x) g
unmarkQuedL g v = fmap (\ x -> if x `elem` v then markF x else x) g


unmark :: Vertex a -> a
unmark = label . label
-------------

makeVertex :: a -> Vertex a
makeVertex x = Mark (Mark x False) False

makeEVertex :: Edge a -> Edge (Vertex a)
makeEVertex = fmap makeVertex

makeGVertex :: Graph a -> Graph (Vertex a)
makeGVertex = fmap makeVertex

labelEdge :: Edge (Vertex a) -> Edge a
labelEdge = fmap unmark

labelGraph :: Graph (Vertex a) -> Graph a
labelGraph = fmap unmark

crit :: Vertex a -> Bool
crit = mark.label

nonCritG :: Graph (Vertex a) -> [Vertex a]
nonCritG = filter (not.crit) . vertices

nonCrit :: Eq a => [Vertex a] -> [Vertex a]
nonCrit = filter (not.crit)

scytheGraph :: Eq a => Graph a -- diag - diagram (graph) for which we want to compute the morse data
                    -> Graph a -- morse data of the graph diag
scytheGraph g = (labelGraph . snd)  $ scytheGraphAux ( [] , makeGVertex g )
-- this is gonna be a recursive procedure, so we better make it tail-recursive

scytheGraphAux :: Eq a => ( [Vertex a] , Graph(Vertex a) ) -> ( [Vertex a] , Graph(Vertex a) )
scytheGraphAux ( q , g )
    | null $ nonCritG g = ( [] , g ) -- termination
    | null q            = scytheGraphAux ( [c] , bigD )-- the queue has been emptied, new big loop
    | otherwise         = scytheGraphAux ( nQueue , smallD ) -- queue has elements, execute the loop
    where
        ----------- in case where the queue is empty
        (c:_) = filter (null . nonCrit . incoming g) (nonCritG g) -- minimal non-critical element
        bigD = markQued (markCrit g  c) c -- g with c marked as critical and queued
        ----------- for the remaninig cases
        (y:ys) = q
        nQueue = ys `union` filter (not.mark) ms
        xs = nonCritUnder g y
        (smallD , ms) = if length xs  == 1 && (not.crit) y
                 then let (x:_) = xs in
                     (reducePair g x y , outgoing g y `union` outgoing g x)
                 else (g , outgoing g y)

reducePair :: Eq a => Graph (Vertex a) -> Vertex a -> Vertex a -> Graph (Vertex a)
reducePair g x y = foldl addEdge (markQuedL g' (outgoing g x ++ outgoing g y)) (
    [edge (w,z) | w <- incoming g y \\ [x] ,  z <- outgoing g x \\ [y] ] ++ -- add relations described by reducepair
    [edge (w,z) | w <- incoming g x ,  z <- outgoing g x \\ [y] ] ++ -- preserve second order relations that the removal destroys
    [edge (w,z) | w <- incoming g y \\ [x] ,  z <- outgoing g y ] ++ -- preserve second order relations that the removal destroys
    [edge (w,z) | w <- incoming g x ,  z <- outgoing g y ] -- preserve third order relations that the removal destroys
    -- the presheaf condition makes sure that all these operations make sense
    )
    where
    g' = remVertexL g [x,y]

outgoingE,incomingE,includingE :: Eq a => Graph a -> a -> [Edge a]
incomingE g x = map (flip Edge x) (incoming g x)
outgoingE g x = map (Edge x) (outgoing g x)
includingE g x = incomingE g x `union` outgoingE g x

nonCritUnder :: Eq a => Graph (Vertex a) -> Vertex a -> [Vertex a]
nonCritUnder g v = nonCrit $ incoming g v

