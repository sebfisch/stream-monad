import Criterion.Main
import Control.Monad
import Control.Monad.Stream

main :: IO ()
main = defaultMain
  [ bench "permsort" $ nf (toList . permSort) ([1..4]++[8,7..5]),
    bench "8 queens" $ nf (toList . nQueens) 8 ]

permSort :: [Int] -> Stream [Int]
permSort xs = do ys <- permute xs
                 guard (ascending ys)
                 return ys

permute :: [a] -> Stream [a]
permute [] = return []
permute xs = do (y,ys) <- select xs
                zs <- permute ys
                return (y:zs)

select :: [a] -> Stream (a,[a])
select []     = mzero
select (x:xs) = return (x,xs)
        `mplus` do (y,ys) <- select xs
                   return (y,x:ys)

ascending :: [Int] -> Bool
ascending []       = True
ascending [_]      = True
ascending (x:y:zs) = x <= y && ascending (y:zs)

nQueens :: Int -> Stream [Int]
nQueens n = do qs <- permute [1..n]
               guard (safe qs)
               return qs

safe :: [Int] -> Bool
safe qs = and [ j-i /= abs (qj-qi) | (i,qi) <- iqs, (j,qj) <- iqs, i < j ]
 where iqs = zip [1..] qs
