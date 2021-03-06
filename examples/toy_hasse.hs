
import Math.Scythe

main :: IO ()
main = putStrLn (
        "Hi, and welcome to this mostly pointless example.\n" ++
        "Please take the time to checkout one of our examples\n"
        )
        >> innerLoop

innerLoop = putStrLn (
        "just write \"s1\", \"f8\" , \"t2\" or \"os1\" to get the result.\n" ++
        "type \"exit\" to quit."
        )
        >> getLine
        >>= ( \ x -> if x == "exit"
            then putStrLn "thank you for your visit and have a wonderfull day!"
            else (putStrLn . showEx) x >> innerLoop
                )

showEx :: String -> String
showEx s = case s of "s1"  -> "\n the poset underlying a cell structure of the circle with 4 cells is represented by: \n" ++ show(s1) ++ "\nafter reducing it with scythe we get:\n" ++ show(scythe s1) ++ "\n"
                     "f8"  -> "\n the poset underlying a cell structure of the figure 8 with 3 cells is represented by: \n" ++ show(f8) ++ "\nafter reducing it with scythe we get:\n" ++ show(scythe f8) ++ "\n"
                     "os1" -> "\n the poset underlying a cell structure of the circle with 14 cells is represented by: \n" ++ show(os1) ++ "\nafter reducing it with scythe we get:\n" ++ show(scythe os1) ++ "\n"
                     "t2"  -> "\n the poset underlying a cell structure of the torus with 4 cells is represented by: \n" ++ show(t2) ++ "\nafter reducing it with scythe we get:\n" ++ show(scythe t2) ++ "\n"
                     "cil" -> "\n the poset underlying a cell structure of the cilinder with 4 cells is represented by: \n" ++ show(cil) ++ "\nafter reducing it with scythe we get:\n" ++ show(scythe cil) ++ "\n"
                     _     -> "\n invalid input, care to try again?" ++ "\n"

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

-- cell structure of the 2-torus
t2 :: HasseDiagram String
t2 = HasseD
    [ Hasse "0c1" [] ["1c1","1c2"]
    , Hasse "1c1" ["0c1"]  ["2c1"]
    , Hasse "1c2" ["0c1"]  ["2c1"]
    , Hasse "2c1" ["1c1","1c2"] []
    ]

e1 :: HasseDiagram String
e1 = HasseD
    [ Hasse "0" [] ["01","02"]
    , Hasse "1" [] ["01","12"]
    , Hasse "2" [] ["02","12"]
    , Hasse "01" ["0","1"] ["012"]
    , Hasse "02" ["0","2"] ["012"]
    , Hasse "12" ["0","2"] ["012"]
    , Hasse "012" ["01","02","12"] []
    ]

cil :: HasseDiagram String
cil = HasseD
    [ Hasse "0c1" [] ["1c1","1c3"]
    , Hasse "0c2" [] ["1c2","1c3"]
    , Hasse "1c1" ["0c1"]  ["2c1"]
    , Hasse "1c2" ["0c2"]  ["2c1"]
    , Hasse "1c3" ["0c1","0c2"] ["2c1"]
    , Hasse "2c1" ["1c1","1c2","1c3"] []
    ]
