
module RSA where
import Control.Monad.Fix
import System.Random(randomRIO)
import Data.Bits
import System.Random

-- plan b to generate random numbers
-- randomNumber :: Integer -> Integer -> IO Integer
-- randomNumber a b = (randomRIO(a,b))

isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2.. ceiling (sqrt (fromIntegral k))], k `mod`x  == 0]
    
main x m k= do        
        let s1 = mkStdGen k
        let (i1, s2) = randomR (x, m :: Integer) s1
        return i1 

getRandomPrime :: Integer -> Integer -> Int ->  IO Integer
getRandomPrime m n k= 
     do        
        x <- main m n k
        r <- return (isPrime x )
        
        if r 
            then return x
            else getRandomPrime m n (k+1) 
                 
calculateTotient :: Integer -> Integer -> Int ->  IO Integer
calculateTotient m n k=
     do
       x <- getRandomPrime m n k
       y <- getRandomPrime m n (k + 1003322)
       
       if x /= y
         then return ((x - 1) * (y -1))
         else calculateTotient m n (k*10)

calculateN :: Integer -> Integer -> Int ->  IO Integer
calculateN m n k=
     do
       x <- getRandomPrime m n k
       y <- getRandomPrime m n (k + 1003322)
       return (x * y)

publicKey :: Integer -> Integer -> Int -> Integer -> IO Integer
publicKey m n k e = do
  
  y <- calculateTotient m n k    
  if (gcd e y) == 1
    then return e    
    else publicKey m n k (e + 1)
 
privateKey :: Integer -> Integer -> Int -> Integer -> IO Integer
privateKey m n k e = do
  x <- calculateTotient m n k
  y <- return (fromIntegral(x * 2 + 1)`div`e)
  return y      

  
encrypt :: Integer -> (Integer, Integer) -> Integer
encrypt m (e, n) = ((m^e) `mod` n)

decript :: Integer -> (Integer, Integer) -> Integer
decript c (d, n ) = (c^d `mod` n)

rsaE :: Integer -> Int -> IO Integer -- main function to encript
rsaE  m seed  = do
  e <- publicKey 1 900 seed 3
  n <- calculateN 1 900 seed  
  c <- return (encrypt m (e, n))
  return (encrypt m (e, n))

rsaD :: Integer -> Int ->IO Integer -- main function to decript
rsaD m seed  = do
  e <- publicKey 1 900 seed 3      -- generates two random primes between 1 and 900
  n <- calculateN 1 900 seed
  d <- privateKey 1 900 seed e
  c <- return (encrypt m (e, n))
  return (decript c (d, n))

