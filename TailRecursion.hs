module TailRecursion where --TailRecursion.hs
data Color = Red|Green|Black|Purple|Blue
fibb :: Int -> Int
fibb n = calcFibb 0 1 n

calcFibb :: Int -> Int -> Int -> Int
calcFibb f0 f1 n = if n < 2	
	then (f0+f1)
	else calcFibb f1 (f0+f1) (n-1)

fact :: Int -> Int
fact n = calcFact 1 n

calcFact :: Int -> Int -> Int
calcFact f1 n = if n < 2
	then f1
	else calcFact (f1*n) (n-1)
	
bin :: Int -> Int -> Int
bin n k = calcBin 1 n k

calcBin :: Double -> Int -> Int -> Int
calcBin cn0 n k = if k < 1
	then floor cn0
	else calcBin ((cn0*fromIntegral(n-k+1))/(fromIntegral k)) n (k-1)
	