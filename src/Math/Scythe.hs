module Math.Scythe where

import Data.List(union,(\\))

-- a Hasse diagram is a directed graph associated to a poset where each vertex
-- is a vertex and each edge is a covering relation (we say that x is covered
-- by y if x < y and there are no elements z such that x < z < y) so we may, in
-- these notes employ the words vertex and edge where elment and covering
-- relation are meant, and for this we offer our deepest apologies.

data Hasse a = Hasse { -- element of a hasse diagram of a poset
    hel :: a ,     -- the label of the elment, say, e
    under :: [a] , -- the elements which are covered by e
    over :: [a]    -- the elements which cover e
    }

-- for simplicity equality should only compare labels, since otherwise we have to
-- make too much code comparing the whole structure of each element
instance Eq a => Eq ( Hasse a ) where
    x == y = hel x == hel y

instance Show a => Show (Hasse a) where
    show (Hasse x y z) = "H{" ++ show x ++", " ++ show y ++ ", " ++ show z ++ "}"

data HasseDiagram a = HasseD { -- a hasse diagram
    hlist :: [Hasse a] -- the list of hasse elements in the diagram
    }
    deriving Eq

instance Show a => Show (HasseDiagram a) where
    show (HasseD l) = foldr (\ x y -> show x ++ ";" ++ y) "" l

scythe :: Eq a => HasseDiagram a -- diag - diagram for which we want to compute the morse data
                  -> HasseDiagram a -- morse data of diag
scythe diag = scytheAux diag []

scytheAux :: Eq a => HasseDiagram a -- d  - diagram we are applying scythe to
                     -> [Hasse a]   -- cs - list of critical elements
                     -> HasseDiagram a -- the morse data of d after applying scythe with cs as critical elements
scytheAux d cs | length cs == (length . hlist) d = d -- this is true exactly when there are no non-critical elements left
               | otherwise = let { mc = minNCrit d cs -- find a minimal critical element
                     } in
                        scytheAux (
                            quePlay
                                d
                                [mc] -- the queue always starts with the new minimal critical element
                                (mc:cs) -- we mark mc, the new critical, element as critical
                                [mc] -- we also mark it so that it can not be requeued in this recursion
                                   )  (mc:cs)

quePlay :: Eq a => HasseDiagram a -- d    - the diagram we want to reduce
                   -> [Hasse a]   -- y:ys - the queue of elements of the diagram
                   -> [Hasse a]   -- c    - the list of critical elments
                   -> [Hasse a]   -- m    - the list of elements that were already enqueued
                   -> HasseDiagram a -- the resulting reduced diagram
quePlay d [] _ _ = d -- if the queue is empty then there is nothing left to do and we return the diagram
quePlay d (y:ys) cs ms =
    if length (nonCUnder y cs d) == 1 then -- there is a non-critical element over exactly one other non-critical
        let{ x   = head (nonCUnder y cs d)
           ; nys = ys ++ elemOver x y ms d
           ; nms = ms `union` nys
           ; y' = head $ filter (==y) (hlist d)
        } in
        quePlay
            (reducepair d x y') -- reduce the diagram d with respect to s and y
            nys -- y gets dequeued and we equeue all elements over x and y
            cs  -- the critical set is not changed by this operation
            nms -- add all new enqueued elements to the marked list
    else
        let { nys = ys ++ elemOver' y ms d
            ; nms = ms `union` nys
        } in
        quePlay
            d   -- the diagram is unchanged at this stage
            nys -- enqueue every element over y that hasn't been enqueued yet
            cs  -- the critical elements remain the same
            nms -- mark the enqueued elements

elemOver' :: Eq a => Hasse a           -- y  - selected elment of d
                     -> [Hasse a]      -- ms - list of marked elements in d
                     -> HasseDiagram a -- d  - diagram
                     -> [Hasse a] -- list of non-marked elements in d that cover y
elemOver' y ms d =
    let{ mkd = map hel ms -- list of labels of the marked elements
    }in
        filter -- filter out marked elements from those over y
            ((`elem` (over y \\ mkd)) . hel)
            (hlist d)

elemOver :: Eq a => Hasse a           -- x  - element of d that is covered by y
                    -> Hasse a        -- y  - selected elment of d
                    -> [Hasse a]      -- ms - list of marked elements in d
                    -> HasseDiagram a -- d  - diagram
                    -> [Hasse a] -- list of non-marked elements in d that cover y
elemOver x y ms d =
    let{ mkd = map hel ms  -- list of labels of the marked elements
    }in
        filter -- filter out marked elements from those over both x and y
            ((`elem` ((over x `union` over y) \\ mkd)) . hel)
            (hlist d)

nonCUnder :: Eq a => Hasse a           -- y  - element in the diagram
                     -> [Hasse a]      -- cs - list of critical elements in d
                     -> HasseDiagram a -- d  - diagram
                     -> [Hasse a] -- list of the non-critical elments under y
nonCUnder y cs d =
    let{ cids = map hel cs } in
        filter -- we filter out from the elements covered by y, those that have been marked as critical
            ((`elem` (under y \\ cids)) . hel)
            (hlist d)

minNCrit :: Eq a => HasseDiagram a -- d  - diagram
                    -> [Hasse a]   -- cs - list of critical elments
                    -> Hasse a -- a minimal non critical elment
minNCrit d cs =
    head $ filter -- we filter out those elments that have non-critical elments under them,
            (\ y -> null $ nonCUnder y cs d) -- and take the first one of those
            (hlist d)

reducepair :: Eq a => HasseDiagram a -- dgr - diagram tha we are reducing
                      -> Hasse a     -- x   - covered element
                      -> Hasse a     -- y   - covering element
                      -> HasseDiagram a -- dgr with the covering relation x<y removed
reducepair dgr x y = HasseD $
    map
        (nelem x y) -- associate to each element of the recomputed element without relations to x and y
        (hlist dgr \\ [x,y])
    where
        nelem :: Eq a => Hasse a    -- x' - the covered element in d
                         -> Hasse a -- y' - the element that covers x
                         -> Hasse a -- z  - a third element in the diagram
                         -> Hasse a -- the element z with the necessary alterations to perform the reduction
        nelem x' y' z =
            let { hx = hel x' ; ox =  over x'
                ; hy = hel y' ; uy = under y'
            } in
            Hasse
                (hel z) -- the label is unchanged
                ((under z \\ [hx,hy]) `union` -- x and y are removed from the elements z covers
                    if hx `elem` under z -- if x is covered by z then we have to add the elements
                        then uy \\ [hx] -- covered by y, except for x to the elments it covers
                        else [])
                ((over  z \\ [hx,hy]) `union` -- x and y are removed from the elements that cover z
                    if hy `elem`  over z -- if y covers z then we should add all elements covering x,
                        then ox \\ [hy]  -- except for y, to those covering z
                        else [])

