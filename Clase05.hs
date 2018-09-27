module Clase05 ( eAprox ) where
import Clase02 (signo, max2)
import Clase04 (factorial)


-- TODO: Faltan cosas por que las diapositivas se subieron rotas

--------------------------------------------
-- Diapositiva 4
------------------------------------------

reciprocal :: Integer -> Float
eAprox :: Integer -> Float
e :: Float

parteEntera :: Float -> Integer

-- Una funcion auxiliar
reciprocal x | x == 0 = error "Division por 0"
             | otherwise = 1/(fromInteger x)

eAprox n | n < 0 = error "Fuera de dominio" -- ∉N
         | n == 0 = 1
         | n > 0 = eAprox(n-1) + (reciprocal . factorial) n 
         | otherwise = error "Fuera de dominio de la ∑"

e = eAprox 100

-- Una version secuencial de lo mismo
-- eAprox_f n = sum (map (reciprocal . factorial) [0..n])

-- Version naive recursiva que nos piden.
-- Por que haskell ya tiene ceiling/floor/truncate/round/etc
parteEntera n | (n>=0) && (n<1) = 0 
              | n < 0  = -1 + parteEntera(n+1) 
              | n > 0 =  1 + parteEntera(n-1)

--------------------------------------------
-- Diapositiva 5
------------------------------------------

division :: Integer -> Integer -> (Integer, Integer)
division_z :: Integer -> Integer -> (Integer, Integer)


-- Función naive. Restamos y contamos cuantas veces restamos el divisor al dividendo
-- antes de que lo pasemos, eso nos da un n≠0 tal que n*divisor + algo = dividendo

division num den | den <= 0 = error "El divisor tiene que ser mayor que 0."
                 | den > num = (0, num) 
                 | otherwise = (fst prox_termino + 1, snd prox_termino) -- cociente arranca en n=1
                   where prox_termino =  division (num-den) den

-- Para negativos el código sale con un pequeño ajuste
division_z num den | den <= 0 = error "El divisor tiene que ser mayor que 0."
                   | den > (abs num) = (0, num) 
                   | otherwise = ((fst prox_termino) + s, snd prox_termino) 
                     where prox_termino =  division_z (num - s*den) den
                           s = signo(num)

-------------------------------------------
-- Diapositiva 7
------------------------------------------

-- IDEA: factores hasta √n? en algún lado leí que técnicamente con eso alcanza
-- Buscar una proof
sumarDivisoresHasta :: Integer -> Integer -> Integer
sumarDivisoresHasta_f :: Integer -> Integer
sumaDivisores :: Integer -> Integer

esDivisor :: Integer -> Integer -> Bool

-- Divisores positivos anteriores a n
sumarDivisoresHasta x n | x > n  = error "Error !" 
                        | x == 0 = 0
                        | esDivisor n x = x + sumarDivisoresHasta (x-1) n
                        | otherwise = sumarDivisoresHasta (x-1) n

sumaDivisores n = sumarDivisoresHasta n n

-- Con high order functions
sumarDivisoresHasta_f n = sum (filter (\x -> (rem n x) == 0) [1..n])

esDivisor x y = (rem x y) == 0

-------------------------------------------
-- Diapositiva 8
------------------------------------------
f :: Integer -> Integer -> Integer
g :: Integer -> Integer -> Integer

-- Uso una funcion auxiliar ∑ interna, prolijidad ante todo
g i j | ( i == 0 || j == 0) = 0
      | j > 0 = (i^j) + g i (j-1)

-- La ∑ de afuera.
-- Por cada i∈[1,n] que voy pasando, calculo la sumatoria de adentro y voy sumando los resultados
f n m | (n < 0 || m < 0) = error "Fuera de dominio"
      | (n == 0) = 0
      | (n > 0) = g n m + f (n-1) m -- voy bajando en i

-- Versión high-order functions
ff :: Integer -> Integer -> Integer
ff n m = sum (map (\x -> sum (map (\y -> x^y) [1..m])) [1..n])


-- Ejercicio 4: ∑q^(a+b) con a∈[1,n] y b∈[1,m]
-- sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias_f :: Integer -> Integer -> Integer -> Integer

sumaPotencias_f q n m = sum (map (\a -> sum (map (\b -> q^(a+b)) [1..m])) [1..n])

-- TODO: el código sumaPotencias y sumaRacionales es prácticamente idéntico a estos
-- Auxiliar, doble contador y palo a la bolsa.
