module Clase03  (unidades) where
import Clase02 (esPar, esImpar, esMultiploDe)


--------------------------------------------
-- Diapositiva 2
------------------------------------------

-- dos formas distintas 
unidades :: Integer -> Integer
unidades_r :: Integer -> Integer

sumaUnidades3 :: Integer -> Integer -> Integer -> Integer
todosImpares :: Integer -> Integer -> Integer -> Bool

alMenosUnImpar :: Integer -> Integer -> Integer -> Bool
alMenosDosImpares :: Integer -> Integer -> Integer -> Bool
alMenosDosPares :: Integer -> Integer -> Integer -> Bool
alMenosUnMultiploDe :: Integer -> Integer -> Integer -> Bool


-- la implementación simple y canónica
unidades n = rem (abs n) 10

-- una versión recursiva
unidades_r n | n <= (-10) = (unidades_r . abs) n
             | n >= (10) = unidades_r(n-10)
             | otherwise = n


sumaUnidades3 x1 x2 x3 = (unidades x1) + (unidades x2) + (unidades x3)
todosImpares x1 x2 x3 = (esImpar x1) && (esImpar x2) && (esImpar x3)

alMenosUnImpar x1 x2 x3 = (esImpar x1) || (esImpar x2) || (esImpar x3)


-- Incluyo una forma alternativa por probar, si hay dos pares, la suma no supera 1. 
alMenosDosPares x1 x2 x3 =  (esPar x1 && esPar x2) || (esPar x1 && esPar x3) || (esPar x2 && esPar x3)
alMenosDosPares2 x1 x2 x3 =  (mod (abs x1) 2 + mod (abs x2) 2  + mod (abs x3) 2 ) <= 1

-- Análogamente, con los impares
alMenosDosImpares x1 x2 x3 = (esImpar x1 && esImpar x2) || (esImpar x1 && esImpar x3) || (esImpar x2 && esImpar x3)
alMenosDosImpares2 x1 x2 x3 = (mod (abs x1) 2 + mod (abs x2) 2  + mod (abs x3) 2 ) >= 2

alMenosUnMultiploDe x1 x2 x3 = (esMultiploDe x1 x3) || (esMultiploDe x2 x3)
alMenosUnMultiploDe2 x1 x2 x3 = (mod x1 x3 == 0) || (mod x2 x3 == 0)

-- Por diversión agregué estas, pero high order functions está fuera de lo dado en clase.
-- Así que quedan comentadas
-- alMenosDosImparesf x1 x2 x3 = length(filter esImpar [x1,x2,x3]) >= 2
-- sumaUnidades3f x1 x2 x3 = sum(map unidades [x1,x2,x3])
-- alMenosDosParesf x1 x2 x3 = length(filter esPar [x1,x2,x3]) >= 2

--------------------------------------------
-- Diapositiva 11: Relaciones
------------------------------------------

-- 8) Dados a,b ∈ Z
r1 :: Integer -> Integer -> Bool
r2 :: Integer -> Integer -> Bool
r3 :: Integer -> Integer -> Bool

relacion1 :: Integer -> Integer -> Bool

r1 a b = (esPar a) == (esPar b)
r2 a b = rem (2*a + 3*b) 5 == 0

-- son tres cosas que tienen que ser distintas de manera excluyente entre sí
r3 a b = (unidades a /= unidades b) &&
         (unidades a /= unidades (a*b)) && -- a=1 y b=2 son distintos, pero a*b=b falla
         (unidades b /= unidades (a*b)) -- análogamente ...

relacion1 x y = (r1 x y) || (r2 x y) || (r3 x y)  

-- 9) Dados x,y ∈ R
relacion2 :: Integer -> Integer -> Bool
relacion2 x y = (x<3 && y<3) || (y>=3 && x>=3)

-- 10) Dados x,y ∈ R
relacion3 :: Integer -> Integer -> Bool
relacion3 x y | (x<3 && y<3) = True 
              | (y>=3 && x>=3) && (y<=7 && x<=7 ) = True
              | (x>=7 && y>=7) = True

-- 11) Dados (a,b) y (p,q)
relacion4 :: (Float,Float) -> (Float,Float) -> Bool
relacion5 :: (Integer,Integer) -> (Integer,Integer) -> Bool
relacion6 :: (Integer,Integer) -> (Integer,Integer) -> Bool


{- La relacion nos pide es que a=(k*p) b=(k*q)
 - Entonces la idea es cumplir las dos ecuaciones despejando k de una y viendo si la segunda 
 - verifica. Es decir (a/p)= k y también  k=b/q -}
relacion4 x y | x == (0,0) || y == (0,0) = error "Fuera del Dominio" -- ∉ R-{0}
              | otherwise = (( (fst x)/(fst y) ) * snd(y)) == snd(x)

-- Misma idea, pero en Z como dominio ... otra forma de hacerlo a/p = b/q
relacion5 x y | x == (0,0) || y == (0,0) = error "Fuera del Dominio" -- ∉ R-{0}
              | otherwise = ( div (fst x) (fst y) ) == ( div (snd x) (snd y) )

-- Same, pero incluyo al 0 ... con la unica condición que y no sea 0, pero eso no amerita usar 'error'
relacion6 x y | y == (0,0) = False 
              | otherwise = ( div (fst x) (fst y) ) == ( div (snd x) (snd y) )

-------------
factorial :: Integer -> Integer
factorial n | (n < 0)   = error "Fuera del dominio" -- ∉N
            | (n == 0)  = 1
            | otherwise = n * factorial (n - 1)

-- Esta es otra forma que ví en el Haskell Book.
-- factorial_an 0 = 1
-- factorial_an n = n * factorial (n - 1)

--------------------------------------------
-- Diapositiva 44
------------------------------------------

sc :: Integer -> Integer

fibonacci :: Integer -> Integer

a1 :: Integer -> Integer
a2 :: Integer -> Integer
a3 :: Integer -> Integer

fibonacci n | (n < 0)  = error "Fuera de dominio" -- ∉N
            | (n == 0) = 0
            | (n == 1) = 1
            | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

sc n | (n < 0)   = error "Fuera de dominio" -- ∉N
     | (n == 0)  = 0
     | otherwise = sc (n - 1) + n ^ 2

a1 n | (n < 0)   = error "Fuera de dominio" -- ∉N
     | (n == 1)  = 2
     | otherwise = 2 * a1 (n - 1) + 2 ^ (n) * factorial (n-1)

a2 n | (n < 0)   = error "Fuera de dominio" -- ∉N
     | (n == 1)  = 2
     | (n == 2)  = 2
     | otherwise = (n-2)*a2(n-1) + 2*(n-1)*a2(n-2)

a3 n | (n <= 0)     = error "Fuera de dominio" -- ∉N
     | (n == 1)    = -3
     | (n == 2)    = 6
     | esImpar(n) = (-1) * a3 (n-1) - 3
     | otherwise    = a3 (n-1) + 2 * a3 (n-2) +9
