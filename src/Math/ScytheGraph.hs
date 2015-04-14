module Math.ScytheGraph where

import Math.Graphs
import Data.List((\\),union)

type Vertex a = Marked (Marked a)
-- we mark the vertices twice

markCrit,unmarkCrit :: Vertex a -> Vertex a
markCrit = fmap markT
unmarkCrit = fmap markF

markQued,unmarkQued :: Vertex a -> Vertex a
markQued = markT
unmarkQued = markF

unmark :: Vertex a -> a
unmark = label . label
-------------

makeVertex :: a -> Vertex a
makeVertex x = Mark (Mark x False) False

makeEVertex :: Edge a -> Edge (Vertex a)
makeEVertex (Edge x y) = Edge (makeVertex x) (makeVertex y)

makeGVertex :: Graph a -> Graph (Vertex a)
makeGVertex (Graph vs es) = Graph (map makeVertex vs) (map makeEVertex es)

labelEdge :: Edge (Vertex a) -> Edge a
labelEdge (Edge x y) = Edge (unmark x) (unmark y)

labelGraph :: Graph (Vertex a) -> Graph a
labelGraph (Graph vs es) = Graph (map unmark vs) (map labelEdge es)

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
    | null $ nonCritG g = ( [] , g )
    | null q            = scytheGraphAux ( [c] , bigD ) -- the queue has been emptied, new big loop
    | otherwise         = scytheGraphAux ( nQueue , smallD ) -- queue has elements, execute the loop
    where
        c = head $ filter (null . nonCrit . incoming g) (nonCritG g) -- minimal non-critical element
        bigD = Graph -- g with c marked as critical an queued
               (map (\x -> if x == c then (markQued . markCrit) x else unmarkQued x) (vertices g))
               (edges g)
        nQueue = tail q `union` filter (not.mark) ms
        y = head q
        smallD = if (length . nonCritUnder g) y == 1
                 then reducePair g (head (nonCritUnder g y)) y
                 else g
        ms = if (length . nonCritUnder g) y == 1
             then outgoing g y `union` outgoing g (head $ nonCritUnder g y)
             else outgoing g y

reducePair :: Eq a => Graph (Vertex a) -> Vertex a -> Vertex a -> Graph (Vertex a)
reducePair g x y = Graph
        (map (\ t -> if t `elem` (outgoing g x ++ outgoing g y) then markQued t else t) (vertices g'))
        (edges g' `union` [Edge z w | z <- outgoing g x \\ [y], w <- incoming g y \\ [x]])
    where
    g' = Graph
         (vertices g \\ [x,y])
         (edges g \\ (includingE g x `union` includingE g y))

outgoingE,incomingE,includingE :: Eq a => Graph a -> a -> [Edge a]
incomingE g x = map (flip Edge x) (incoming g x)
outgoingE g x = map (Edge x) (outgoing g x)
includingE g x = incomingE g x `union` outgoingE g x

nonCritUnder :: Eq a => Graph (Vertex a) -> Vertex a -> [Vertex a]
nonCritUnder g v = nonCrit $ incoming g v
