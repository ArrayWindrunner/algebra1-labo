module Clase02 (signo, valorAbsoluto, esPar, esImpar, esMultiploDe, norma2D, max2) where 
import Clase01 (f, doble)

--------------------------------------------
-- Diapositiva 11
------------------------------------------
signo :: Integer -> Integer
max2 :: Integer -> Integer -> Integer
max3 :: Integer -> Integer -> Integer -> Integer
valorAbsoluto :: Integer -> Integer

--------------------------------------------
-- Diapositiva 27+
------------------------------------------
esPar :: Integer -> Bool
esImpar :: Integer -> Bool
esMultiploDe :: Integer -> Integer -> Bool
cuadruple :: Integer -> Integer
norma2D :: (Float, Float) -> Float


signo x | x > 0     = 1
        | x < 0     = -1
        | otherwise = 0

valorAbsoluto x | x > 0     = x
                | otherwise = x * (-1)

max2 x y | x > y     = x
         | otherwise = y

max3 x y z = max2 (max2 x y) z

esPar x = (rem x 2) == 0
esImpar x = (rem x 2) /= 0

esMultiploDe x y = (rem x y) == 0

cuadruple x = (doble . doble) x
triple x = 3 * x

norma2D p = sqrt (f (fst p) (snd p))

--------------------------------------------
-- Diapositiva 44
------------------------------------------
crearPar :: a -> a -> (a, a)
invertirPar :: (a, a) -> (a, a)
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float

crearPar x y = (x, y)
invertirPar t = ((snd t), (fst t))

-- || B - A ||
distanciaPuntos x y = norma2D ((fst y) - (fst x), (snd y) - (snd x))

--------------------------------------------
-- Diapositiva 45
------------------------------------------

f1 :: Float -> (Float, Float, Float)
f2 :: Integer -> Integer
f3 :: Integer -> Integer
f4 :: (Integer, Integer) -> Integer

f1 x = (2 * x, x ^ 2, x - 7)

f2 n | (esPar n) = div n 2
     | otherwise = n + 1

f3 n | (rem n 6 == 0) = div (n ^ 2) 2
     | otherwise      = 3 * n + 1

f4 t = fst (t) * ((snd t) + 1)

