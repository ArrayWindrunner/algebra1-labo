module Clase06 (  ) where
import Clase05 (esPrimo)
import Clase02 (esPar)

-------------------------------------------
-- Diapositiva 14: Ejercicios
------------------------------------------

{-- 
-- Ejercicio 1.a
-- La definición de factorial que nos dan es inválida, por que no se pueden bindear parámetros.
-- 
-- Ejercicio 1.b
-- La definición de "iguales" también es inválida ya que no estamos evaluando
-- valores concretos o pudiendo bindear a variables ya que serían dos variables llamadas x conflictuando.
--}


-- Ejercicio 2
yLogico :: Bool -> Bool -> Bool
oLogico :: Bool -> Bool -> Bool
implica :: Bool -> Bool -> Bool
sumaGaussiana :: Integer -> Integer
algunoEsCero :: (Integer, Integer , Integer) -> Bool
productoInterno :: (Float, Float) ->  (Float, Float) -> Float

-- Sólo es verdadero cuando ambos parámetros
yLogico True True = True
yLogico _ _ = False

-- Solo es falso cuando ambos parámetros lo son
oLogico False False = False
oLogico _ _ = True

-- Sólo es falso cuando  V F
implica True False = False
implica _ _ = True

-- Creo que me piden la suma recursiva y no el n*(n+1)/2
sumaGaussiana 1 = 1
sumaGaussiana n = n + sumaGaussiana (n-1)

-- Err, es breve y cuenta como pattern matching
algunoEsCero (x, y, z) = x*y*z == 0
productoInterno (x1, y1) (x2, y2) =  x1*x2 + y1*y2

-------------------------------------------
-- Diapositiva 15: Ejercicios
------------------------------------------

-- esSumaDeDosPrimos :: Integer -> Bool
sumaDosPrimosAux :: Integer -> (Integer,Integer) -> (Integer,Integer)
esSumaDeDosPrimosf :: Integer -> Bool
esSumaDeDosPrimos :: Integer -> Bool

-- Recorrimos todo y no encontramos nada
sumaDosPrimosAux n (0,_) = (0,0)

-- Es fea la funcion, pero la idea es usar dos contadores x, y
-- Voy bajando en x desde n, y voy subiendo en y desde 1 por cada x
-- Ya que es más probable que pueda escribir un numero como un primo menor y cercano a n + un primo chiquito
sumaDosPrimosAux n par |  n < 0 || x > n || y > n = error "Fuera de dominio"
                       | (esPrimo x) && (esPrimo y) && (x + y == n) = par
                       | (y == n) = sumaDosPrimosAux n (x-1, 1)
                       | otherwise = sumaDosPrimosAux n (x, y + 1)
                         where x = fst par
                               y = snd par

-- La funcion que nos piden
esSumaDeDosPrimos n = fst (sumaDosPrimosAux n (n,n)) /= 0


-- La versión funcional: armo un rango de 1..n y me quedo con los primos, hago producto cartesiano y sumo
-- los pares y me fijo si alguno es igual a n
esSumaDeDosPrimosf n = length (filter ( \t -> (head t + last t) == n  ) (sequence [primos, primos])) > 0
                        where primos = filter esPrimo [1..n]

-- Caso base: 4, primer par > 2 que es suma de dos primos
goldbach :: Integer -> Bool


goldbach 4 = True
goldbach n | esPar n = (esSumaDeDosPrimosf n) && goldbach (n-1)
           | otherwise = goldbach (n-1)

sumaDigitos :: Integer -> Integer

sumaDigitos n | n < 0  = error "Fuera de dominio"
              | n > 10 = resto + sumaDigitos cociente
              | otherwise = resto + cociente
                where resto = rem n 10
                      cociente = div n 10
                      
-- funcional
-- sumaDigitosf = sum . map (read . return) . show


-- Declaro dos funciones auxiliares para el ejercicio 4

-- Cantidad de dígitos de un número: 10^n +1 ≈ nDigitos 
nDigitos :: Integer -> Integer
nDigitos n = truncate (logBase 10 (fromInteger n)) + 1


 -- ∑(u * 10^n) = n
-- Doy un numero, y devuelve la unidad repetida c veces
todosIdenticos n c | c < 0 || n < 0 = error "Fuera de dominio"
                   | c > 0 = (rem n 10) * 10^(c-1) + todosIdenticos n (c-1)
                   | c == 0 = 0


identicosDigitos :: Integer -> Bool

-- Si son todos iguales, puedo agarrar la unidad, armar un numero con k digitos repitiendola y ver si es n
identicosDigitos n  | n < 0  = error "Fuera de dominio"
                    | otherwise = n == todosIdenticos (rem n 10) (nDigitos n)


-- Supongo que lo que nos piden, es algo análogo a goldbach, que es ver si se verifica hasta un n
-- Como no nos indican signature, devuelvo la cantidad de términos
collatzMax :: Integer -> (Integer, Integer) -> (Integer, Integer)
collatzAux :: Integer -> Integer -> Integer
collatz :: Integer -> Integer

collatz n = collatzAux n 0

collatzAux 1 c = c + 1 -- hay que contar el 1
collatzAux n c | n < 0 = error "Fuera de dominio"
               | esPar (n) = collatzAux (div n 2) (c+1)
               | otherwise = collatzAux (3 * n + 1) (c+1)

-- Básicamente buscamos el n que genera el c (cantidad de términos) más grande
-- y devolvemos el par (n,c). Para 10.000 (lo que pide consigna) es n=6171 c=262
collatzMax 1 par_nc = par_nc
collatzMax hasta (n,c) | terminos > c = collatzMax (hasta-1) (hasta, terminos)
                       | otherwise = collatzMax (hasta-1) (n,c)
                         where terminos = collatz hasta

