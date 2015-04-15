module Math.CGraphs where

import Data.List(union,(\\))

data CVertex a = CVert { label :: a , crit :: Bool }

instance Eq a => Eq (CVertex a) where
    v == v' = label v == label v'

instance Show a => Show (CVertex a) where
    show v = show (label v) ++ (if crit v then "*" else [])

data CEdge a = Edge { from :: CVertex a, to :: CVertex a }
    deriving Eq

instance Show a => Show (CEdge a) where
    show (Edge v1 v2) = "(" ++ show v1 ++ "--" ++ show v2 ++ ")"

data CGraph a = CGraph { vertices :: [CVertex a] , edges :: [CEdge a]}

instance Show a => Show (CGraph a) where
    show (CGraph vs es) = show vs ++ "\n" ++ show es

newCGraph :: CGraph a
newCGraph = CGraph [] []

incoming, outgoing, cUnder, ncUnder, cOver, ncOver :: Eq a => CGraph a -> CVertex a -> [CVertex a]
incoming g v = map from (filter ((==v) . to)   (edges g))
outgoing g v = map to   (filter ((==v) . from) (edges g))
cUnder   g v = filter crit       (incoming g v)
ncUnder  g v = filter (not.crit) (incoming g v)
cOver    g v = filter crit       (outgoing g v)
ncOver   g v = filter (not.crit) (outgoing g v)

addCEdge :: Eq a => CGraph a -> CEdge a -> CGraph a
addCEdge g e = CGraph (vertices g `union` [from e,to e]) (edges g `union` [e])

markCrit :: Eq a => CGraph a -> CVertex a -> CGraph a
markCrit g v = CGraph ((vertices g \\ [v]) ++ [CVert (label v) True]) (edges g)


