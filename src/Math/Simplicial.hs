module Math.Simplicial where

-- import Math.Graphs
import Control.Monad(join)
import Data.List(union)
import Math.Graphs

-- a simplex is a list of vertices

type Simplex a = [a]

type SimpComplex a = [Simplex a]

complex :: Eq a => [Simplex a] -> SimpComplex a
complex = union [] . join . map subsimplices

subsimplices :: Simplex a -> SimpComplex a
subsimplices [] = [[]]
subsimplices [x] = [[x]]
subsimplices (x:xs) = map (x:) (subsimplices xs) ++ subsimplices xs ++ [[x]]

subsimplexGraph :: Eq a => Simplex a -> Graph (Simplex a)
-- returns the graph of the simplicial complex associated to a single simplex
subsimplexGraph [] = newGraph
subsimplexGraph [x] = addVertex newGraph [x]
subsimplexGraph s = foldl
    addEdge
    (uniteGraphs $ map subsimplexGraph maxSubsimplices)
    [Edge ss s | ss <- maxSubsimplices ]
    where
    maxSubsimplices = filter ((== length s -1) . length) (subsimplices s)

complexGraph :: Eq a => SimpComplex a -> Graph (Simplex a)
-- returns the graph of a simplicial complex associated to a list of (assumed) top-dimensional simplices
complexGraph = uniteGraphs . map subsimplexGraph
