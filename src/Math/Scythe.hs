module Math.Scythe where
-- this is almost as long as my xmonad.hs

import Data.List(union,(\\))

data Hasse a = Hasse { hel :: a , under :: [a] , over :: [a] }

instance Eq a => Eq ( Hasse a ) where
    x == y = hel x == hel y

instance Show a => Show (Hasse a) where
    show (Hasse x y z) = "H{" ++ show x ++", " ++ show y ++ ", " ++ show z ++ "}"

data HasseDiagram a = HasseD { hlist :: [Hasse a] }
    deriving Eq

instance Show a => Show (HasseDiagram a) where
    show (HasseD l) = foldr (\ x y -> show x ++ ";" ++ y) "" l

s1 :: HasseDiagram String
s1 = HasseD
    [ Hasse "0c1" [] ["1c1","1c2"]
    , Hasse "0c2" [] ["1c1","1c2"]
    , Hasse "1c1" ["0c1","0c2"] []
    , Hasse "1c2" ["0c1","0c2"] []
    ]

os1 :: HasseDiagram String
os1 = HasseD
    [ Hasse "0c1" [] ["1c7","1c1"]
    , Hasse "0c2" [] ["1c1","1c2"]
    , Hasse "0c3" [] ["1c2","1c3"]
    , Hasse "0c4" [] ["1c3","1c4"]
    , Hasse "0c5" [] ["1c4","1c5"]
    , Hasse "0c6" [] ["1c5","1c6"]
    , Hasse "0c7" [] ["1c6","1c7"]
    , Hasse "1c1" ["0c1","0c2"] []
    , Hasse "1c2" ["0c2","0c3"] []
    , Hasse "1c3" ["0c3","0c4"] []
    , Hasse "1c4" ["0c4","0c5"] []
    , Hasse "1c5" ["0c5","0c6"] []
    , Hasse "1c6" ["0c6","0c7"] []
    , Hasse "1c7" ["0c7","0c1"] []
    ]

f8 :: HasseDiagram String
f8 = HasseD
    [ Hasse "1"  [] ["2","2'"]
    , Hasse "2"  ["1"] []
    , Hasse "2'" ["1"] []
    ]

e1 :: HasseDiagram String
e1 = HasseD
    [ Hasse "1"  [] ["2'"]
    , Hasse "1'" [] ["2","2'"]
    , Hasse "2"  ["1'"] []
    , Hasse "2'" ["1","1'"] []
    ]

scythe :: Eq a => HasseDiagram a -> HasseDiagram a
scythe diag = scytheAux diag []

scytheAux :: Eq a => HasseDiagram a -> [Hasse a] -> HasseDiagram a
-- takes a diagram and a list of critical elements, it then uses the queue
-- procedure to generate the result of scythe in case there are still
-- non-critical elements
scytheAux d cs | length cs == (length . hlist) d = d -- note that this is true exactly when there are no non-critical elements left
               | otherwise = let { mc = minNCrit d cs } in
                     scytheAux (quePlay d [mc] (mc:cs) [mc])  (mc:cs)

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

-------------------------------------------------------

elemOver' :: Eq a => Hasse a -> [Hasse a] -> HasseDiagram a -> [Hasse a]
elemOver' y ms d =
    let{ mkd = map hel ms }in
        filter
            ((`elem` (over y \\ mkd)) . hel)
            (hlist d)

elemOver :: Eq a => Hasse a -> Hasse a -> [Hasse a] -> HasseDiagram a -> [Hasse a]
elemOver x y ms d =
    let{ mkd = map hel ms }in
        filter
            ((`elem` ((over x `union` over y) \\ mkd)) . hel)
            (hlist d)

nonCUnder :: Eq a => Hasse a -> [Hasse a] -> HasseDiagram a -> [Hasse a]
nonCUnder y cs d =
    let{ cids = map hel cs } in
        filter
            ((`elem` (under y \\ cids)) . hel)
            (hlist d)

minNCrit :: Eq a => HasseDiagram a -> [Hasse a] -> Hasse a
minNCrit d cs =
    head $ filter
            (\ y -> null $ nonCUnder y cs d)
            (hlist d)

reducepair :: Eq a => HasseDiagram a -> Hasse a -> Hasse a -> HasseDiagram a
reducepair dgr x y = HasseD $
    map
        (nelem x y)
        (hlist dgr \\ [x,y])
    where
        nelem :: Eq a => Hasse a -> Hasse a -> Hasse a -> Hasse a
        nelem x' y' z =
            let { hx = hel x' ; ox =  over x'
                ; hy = hel y' ; uy = under y'
            } in
            Hasse
                (hel z)
                ((under z \\ [hx,hy]) `union` if hx `elem` under z then uy \\ [hx] else [])
                ((over  z \\ [hx,hy]) `union` if hy `elem`  over z then ox \\ [hy] else [])

