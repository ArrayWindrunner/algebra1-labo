module Clase04 ( esPar_r2, esPar_z ) where
import Clase02 (esPar, esImpar)

-- Ejemplo pre-ejercicios
factorial :: Integer -> Integer

factorial n | n < 0  = error "Fuera de dominio. Ver: funcion gamma" -- ∉N
            | n == 0 = 1
            | otherwise = n * factorial(n-1)

--------------------------------------------
-- Diapositiva 15
------------------------------------------

fib :: Integer -> Integer
a1 :: Integer -> Integer
a2 :: Integer -> Integer
a3 :: Integer -> Integer


fib n | n < 0 = error "Fuera de dominio" -- ∉N
      | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1)+fib(n-2)

a1 n | n < 1 = error "Fuera de dominio" -- ∉N
     | n == 1 = 2
     | otherwise = 2*(n-1)*a1(n-1) + (2^n)*factorial(n-1)

a2 n | n < 1  = error "Fuera de dominio" -- ∉N
     | n == 1 = 1
     | n == 2 = 2
     | otherwise = (n-2)*a2(n-1) + 2*(n-1)*a2(n-2)

-- Uso el viejo esPar no-recursivo para no castigar más al pobre stack :(
a3 n | n < 1  = error "Fuera de dominio" -- ∉N
     | n == 1 = (-3)
     | n == 2 = (6)
     | esPar n = (-1) * a3(n-1) - 3
     | esImpar n = a3(n-1) + 2*a3(n-2) + 9

--------------------------------------------
-- Diapositiva 19
------------------------------------------

-- Nota: para R uso Double en vez de Float
sumatoria :: Integer -> Integer
f1 :: Integer -> Integer
f2 :: Integer -> Float -> Float

-- FIXME: Estos ejercicios nos dijeron que tenian error en la diapositiva.
-- Cuando estén actualizadas, los corrijo
-- f3 :: Integer -> Double -> Double
-- f4 :: Integer -> Double -> Double 

-- ∑n
sumatoria n | n < 0 = error "Fuera de dominio" -- ∉N
            | n == 0 = 0
            | n > 0 = n + sumatoria(n-1)

-- ∑(2^n)
f1 n | n <  0 = error "Fuera de dominio" -- ∉N
     | n == 0 = 1
     | n >  0 = (2^n) + f1(n-1)

-- ∑(q^n)
f2 n q | n <  0 = error "Fuera de dominio" -- ∉N
       | n == 0 = 1
       | n >  0 = (q^n) + f2 (n-1) q

-- ∑(q^n) | f2 10 7 = map(\n -> 7^n) [0..10]  por ejemplo
f2 n q | n <  0 = error "Fuera de dominio" -- ∉N
       | n == 0 = 1
       | n >  0 = (q^n) + f2 (n-1) q

-- ∑(q^n) de 1 a 2n
-- f3 n q = f2 (2*n) q

--------------------------------------------
-- Diapositiva 20
------------------------------------------
esPar_r1 :: Integer -> Bool
esPar_r2 :: Integer -> Bool
esPar_z :: Integer -> Bool
esMultiploDe3_r :: Integer -> Bool

sumaImpares_r :: Integer -> Integer
doblefact_r :: Integer -> Integer


-- Interesante lazyness/redux thing happening right here :D
-- esto es respetando lo que dice la diapositiva, pero opino que la función "se rompe" con numeros negativos ⊥
esPar_r1 n | n == 0 = True
          | n == 1 = False
          | otherwise = esPar_r1(n - 2)

-- Esta es la otra versión que plantea la diapo, ídem situación con los negativos
esPar_r2 n | n == 0 = True
         | otherwise = not (esPar_r2 (n-1))


-- Esta es la versión que se me ocurrió (usando módulos) que acepta negativos, y que usa sólo lo visto en clase. 
--  revisé rápido con length ( filter isTrue (map esPar_z [(-20)..20])) y dá 21 números, que es correcto
esPar_z n | n == 0 = True
          | (abs n) == 1 = False
          | (abs n) > 0 = esPar_z((abs n) - 2)

-- 
esMultiploDe3_r n | n < 0 = error "Fuera de dominio"-- ∉N
                  | n == 0 = True
                  | n == 1 = False
                  | otherwise = esMultiploDe3_r(n - 3)

sumaImpares_r n | n < 0 = error "Fuera de dominio" -- ∉N
                | n == 0 = 0
                | (esImpar n) = n + sumaImpares_r (n-1)
                | otherwise   = 0 + sumaImpares_r (n-1)

-- La idea del factorial doble es hacer productoria con todos los x∈N anteriores de
-- misma paridad, entonces
doblefact_r n | n < 0  = error "Fuera de dominio"-- ∉N
              | n <= 1 = 1 -- según paridad va a 0 o 1 en el último término
              | esPar n = n * doblefact_r (n-2)
              | esImpar n = n * doblefact_r (n-2)

-- Por diversión, una versión secuencial usando high order functions
-- doblefact_f n = product (filter (\x -> esPar x == esPar n) [1..n])