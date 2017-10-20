import Data.List
n = 8

inSameDiag :: ((Int,Int),(Int,Int)) -> Bool

inSameDiag ((a,b),(c,d)) = ((abs(a - c)) == (abs(b - d)))



noSharedDiags :: [Int] -> Bool
noSharedDiags list =
        let pairs x = [(a,b)| a<-x,b<-x,a>b]
            pairCoords = pairs (bindRows (list))
        in  all (not.inSameDiag) pairCoords

bindRows:: [Int] -> [(Int,Int)]
bindRows = zip [1..]

possibilities = permutations [1..n]
solutions = [x | x<-possibilities, noSharedDiags x] --final math computation stored here

out = map stringcoords outcoords
        where outcoords = (bindRows) <$> solutions
              stringcoords:: [(Int,Int)] -> String
              stringcoords l = foldl (++) " " (coordstring <$> l)
              coordstring:: (Int,Int) ->String
              coordstring (a,b) = "(" ++ (show a) ++ ","  ++(show b) ++") "

main = do
        mapM print out
        putStrLn " Complete "
        _ <- getLine --prevent console from closing
        return ()
