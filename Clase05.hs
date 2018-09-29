module Clase05 ( eAprox ) where
import Clase02 (signo, esImpar)
import Clase04 (factorial)


-------------------------------------------
-- Diapositiva 2
------------------------------------------

sumaLosPrimerosNImpares  :: Integer -> Integer

-- Esto también sale : sum (filter esImpar [1..n])
sumaLosPrimerosNImpares n | n <  0 = error "Fuera de dominio"
                          | n == 1 = 1
                          | esImpar n = n + sumaLosPrimerosNImpares (n-1)
                          | otherwise = sumaLosPrimerosNImpares (n-1)

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
-- antes de que lo pasemos, eso nos da un n tal que n*divisor + algo = dividendo
 
division dividendo divisor | divisor <= 0 = error "El divisor tiene que ser mayor que 0."
                           | divisor > dividendo = (0, dividendo) 
                           | otherwise = (fst prox_termino + 1, snd prox_termino) -- cociente arranca en n=1
                             where prox_termino =  division (dividendo-divisor) divisor

-- Para negativos la cosa es distinta, ya que a/d tiene que satisfacer la ecuación a = n * d + r con 0 ≤ r < |d|.
-- Si a < 0, hay que prevenir un resto negativo. Ver apunte Álgebra I de Teresa Krick p115 para explicación matemática
division_z a d | a < 0 = ( s + s * fst division_positiva, (abs d) + s * snd division_positiva)
               | otherwise = ( signo d * fst division_positiva, snd division_positiva )
                 where division_positiva = division (abs a) (abs d)
                       s = signo a

-------------------------------------------
-- Diapositiva 7
------------------------------------------

sumarDivisoresHasta :: Integer -> Integer -> Integer
sumarDivisores_f :: Integer -> Integer
sumarDivisores :: Integer -> Integer

esDivisor :: Integer -> Integer -> Bool

-- Función auxiliar: suma de los divisores de n (sean primos o compuestos), menores al parámetro x.
sumarDivisoresHasta x n | x > n || n < 0 || x < 0 = error "Fuera de dominio !" 
                        | x == 0 = 0
                        | esDivisor n x = x + sumarDivisoresHasta (x-1) n
                        | otherwise = sumarDivisoresHasta (x-1) n

sumarDivisores n = sumarDivisoresHasta n n

-- Con high order functions
sumarDivisores_f n = sum (filter (\x -> (rem n x) == 0) [1..n])

esDivisor x y = (rem x y) == 0

-------------------------------------------
-- Diapositiva 8
------------------------------------------

menorDivisor :: Integer -> Integer
menorDivisorHasta :: Integer -> Integer -> Integer
esPrimo :: Integer -> Bool

-- Arranco con n, y para abajo busco divisores y fijándome el minimo. Si llego a n=1 devuelvo n.
menorDivisorHasta n x | x > n || x <= 0 || n <= 0 = error "Fuera de dominio" -- n∉N
                      | x == 1 = n
                      | esDivisor n x = min x (menorDivisorHasta n (x-1))
                      | otherwise = menorDivisorHasta n (x-1)

-- Con recortar la búsqueda hasta √n, alcanza
menorDivisor n | n <= 0  = error "Fuera de dominio" -- n∉N
               | otherwise = menorDivisorHasta n x_max 
                where x_max  = (parteEntera . sqrt . fromInteger) n

esPrimo n | n < 0 = (esPrimo . abs) n -- Hay que aceptar negativos
          | n == 1 || n == 0 = False
          | otherwise = n == menorDivisor n

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


-- Ejercicio 4: ∑∑q^(a+b) con a∈[1,n] y b∈[1,m]
-- Nótese que el ejercicio no pone restricciones para los casos donde a=b
-- así que hay términos repetidos en la suma
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias_f :: Integer -> Integer -> Integer -> Integer

-- para un a determinado, recorro todo el rango de b's 
sumaPotenciasAux q a b | (q < 0 || a < 0 || b < 0) = error "Fuera de dominio"
                       | b == 0 = 0 
                       | otherwise = q^(a+b) + sumaPotenciasAux q a (b-1) 

-- q es fijo, voy bajando en a y de recorrer todo b para cada "a" se encarga sumaPotenciasAux
sumaPotencias q n m | (q < 0 || n < 0 || m < 0) = error "Fuera de dominio"
                    | (n == 0) = 0
                    | (n > 0) = sumaPotenciasAux q n m  + sumaPotencias q (n-1) m

-- Lo mismo, pero más lindo haciendo AxB con sequence
sumaPotencias_f q n m = sum (map (\ab-> q^(head ab + last ab)) (sequence [[1..n] , [1..m]]))

sumaRacionales :: Integer -> Integer -> Float
sumaRacionalesAux :: Integer -> Integer -> Float


-- Lo que me piden
sumaRacionalesAux n m | n < 0 || m < 0 = error "Fuera de dominio" -- n,m ∉ N
                      | m == 0 = 0
                      | otherwise = (fromInteger n / fromInteger m)  + sumaRacionalesAux n (m-1)

sumaRacionales n m | n < 0 || m < 1 = error "Fuera de dominio"  -- n,m ∉ N
                   | n == 0 = 0
                   | otherwise = sumaRacionalesAux n m + sumaRacionales (n-1) m


-- La versión más potable
sumaRacionales_f n m = sum (map (\x-> head x / last x) (sequence [[1..n] , [1..m]]))


