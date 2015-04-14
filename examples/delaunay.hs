import Math.Graphs
import Math.Scythe
import Data.ByteString.Char8

main :: IO ()
main = putStrLn (
        "Hi, and welcome to this mostly pointless example.\n" ++
        "how many samples would you like to use?"
        )
        >> getLine >>= (\ s ->
            putStrLn "how many dimensions should be sampled?"
            >> getLine
            >>= (randomDT (read s) . read ))
        >>= (\ t ->
            putStrLn "the triangulation has the following list of top-dimensional simplices"
            >> (print . length) t
            >> putStrLn "the resulting complex has"
            >> (return . complex) t)
        >>= (\ x ->
            (putStrLn $ (show . length . vertices) x ++ " cells, and")
            >> (putStrLn $ (show . length . edges) x ++ " relations"))

