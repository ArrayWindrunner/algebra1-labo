module Clase07 (  ) where
import Clase02 (esPar, esMultiploDe)

-------------------------------------------
-- Diapositiva 9-16: Ejercicios
------------------------------------------

listar :: a -> a -> a -> [a]
listaDecreciente :: Integer -> [Integer]
sumatoria :: [Integer] -> Integer
perteneceA :: Integer -> [Integer] -> Bool

listar x y z = [x,y,z]

listaDecreciente (-100) = [(-100)]
listaDecreciente n =  [n] ++ (listaDecreciente (n-1)) 

sumatoria [] = 0
sumatoria (h:t) = h + sumatoria t

perteneceA _ [] = False
perteneceA n (h:t) | n == h = True
                   | otherwise = perteneceA n t

-------------------------------------------
-- Diapositiva 17: Ejercicios
------------------------------------------

-- Van todos de corrido y sin comentarios, son un embole bárbaro

productoria :: [Integer] -> Integer
productoria2 :: [Integer] -> Integer

productoria lista | length lista == 0 = 1
                  | otherwise = (head lista) * (productoria .tail) lista

productoria2 [] = 1
productoria2 (h:t) = h * productoria2 t


sumarN :: Integer -> [Integer] -> [Integer]
sumarN2 :: Integer -> [Integer] -> [Integer]

sumarN n lista | length lista == 0 = []
               | otherwise = (n+ head lista) : sumarN n (tail lista)

sumarN2 _ [] = []
sumarN2 n (h:t) = (n+h) : sumarN n t


sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo2 :: [Integer] -> [Integer]

sumarElUltimo lista | length lista == 0 = []
                    | otherwise = (head lista + last lista) : (sumarElUltimo . tail) lista

sumarElUltimo2 [t] = [2 * t]
sumarElUltimo2 (h:t) = (h + last t) : sumarElUltimo2 t

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero2 :: [Integer] -> [Integer]

-- No nos piden listas ordenadas, así que sirve
sumarElPrimero lista = sumarElUltimo ( (tail lista) ++ [head lista] )
sumarElPrimero2 (h:t) = sumarElUltimo  (t ++ [h])

pares :: [Integer] -> [Integer]
pares2 :: [Integer] -> [Integer]

pares lista | length lista == 0 = []  
            | esPar (head lista) = (head lista) : pares (tail lista)
            | otherwise = pares (tail lista)

pares2 [] = []
pares2 (h:t) | esPar(h) = h : (pares2 t)
             | otherwise = pares2 t

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN2 :: Integer -> [Integer] -> [Integer]

multiplosDeN n lista | length lista == 0 = []
                     | esMultiploDe (head lista) n = (head lista)  : multiplosDeN n (tail lista)
                     | otherwise = multiplosDeN n (tail lista)

multiplosDeN2 _ [] = []
multiplosDeN2 n (h:t) | esMultiploDe h n = h : multiplosDeN2 n t
                      | otherwise = multiplosDeN2 n t 

quitar :: Integer -> [Integer] -> [Integer]
quitar2 :: Integer -> [Integer] -> [Integer]

quitar n lista | length lista == 0 = []
               | n == (head lista) = tail lista
               | otherwise =  (head lista) : quitar n (tail lista)

quitar2 _ [] = []
quitar2 n (h:t) | n == h = t
                | otherwise = h : quitar2 n t

hayRepetidos :: [Integer] -> Bool
hayRepetidos2 :: [Integer] -> Bool

hayRepetidos lista | length lista == 0 = False
                   | perteneceA (head lista) (tail lista) = True
                   | otherwise  = hayRepetidos (tail lista)

hayRepetidos2 [] = False
hayRepetidos2 (h:t) | perteneceA h t = True
                    | otherwise = hayRepetidos t


eliminarRepetidos :: [Integer] -> [Integer]
eliminarRepetidos2 :: [Integer] -> [Integer]

eliminarRepetidos lista | length lista == 0 = []
                        | perteneceA (head lista) (tail lista) = eliminarRepetidos (tail lista)
                        | otherwise = (head lista) : eliminarRepetidos (tail lista)

eliminarRepetidos2 [] = []
eliminarRepetidos2 (h:t) | perteneceA h t = eliminarRepetidos2 t
                         | otherwise = h : eliminarRepetidos2 t


maximo :: [Integer] -> Integer
maximo2 :: [Integer] -> Integer

maximo lista | length lista == 0 = 0
             | otherwise = max (head lista) (maximo (tail lista))

maximo2 [] = 0
maximo2 (h:t) = max h (maximo2 t)

ordenar :: [Integer] -> [Integer]
ordenar2 :: [Integer] -> [Integer]
minimo :: [Integer] -> Integer

minimo [x] = x
minimo (h:t) = min h (minimo t)

ordenar lista | length lista == 1 = [head lista]
              | h < min = h : (ordenar t)
              | otherwise = min : ordenar (quitar min lista)
                where h = head lista
                      t = tail lista 
                      min = minimo t

ordenar2 [x] = [x]
ordenar2 (h:t)  | h < min = h : (ordenar t)
                | otherwise = min : ordenar2 (quitar min (h:t) )
                  where min = minimo t
