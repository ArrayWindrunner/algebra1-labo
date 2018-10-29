module Clase08 ( ) where
import Clase07 ()

-------------------------------------------
-- Diapositiva : Previa
------------------------------------------

-- Por convención de la materia, vamos a usar esto como tipo de dato de conjunto
type Set a = [a]

-- En vez de importar de Clase07, voy a re-definir algunas funciones para darles más generalidad

perteneceA :: (Eq a) => a -> Set a -> Bool
quitarDe :: (Eq a) => a -> Set a -> Set a

perteneceA _ [] = False
perteneceA n (h:t) | n == h = True
                   | otherwise = perteneceA n t

quitarDe _ [] = []
quitarDe n (h:t) | n == h = t
                 | otherwise = h : quitarDe n t

-- Opto por usar typeclasses, por que son operaciones fundamentales, quizá las reuse
agregar :: (Eq a) => a -> Set a -> Set a
vacio :: (Eq a) => Set a

vacio = []

agregar e lista | perteneceA e lista = lista
                | otherwise = e : lista


-------------------------------------------
-- Diapositiva : 10
------------------------------------------

incluido :: Set Integer -> Set Integer -> Bool
iguales  :: Set Integer -> Set Integer -> Bool

-- El elemento vacío pertenece a todo conjunto, y nada es subconjunto de vacío
incluido [] _ = True
incluido _ [] = False

incluido (e:t) referencial  | perteneceA e referencial = True && incluido t referencial
                            | otherwise = False

-- Por definicion: A⊆B y B⊆A
iguales a b = incluido a b && incluido b a


-------------------------------------------
-- Diapositiva : 11
------------------------------------------

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
partes :: Integer -> Set (Set Integer)

-- [ agregar n x | x <- lista ]
agregarATodos _ [] = []
agregarATodos n (conjunto:t) = agregar n conjunto :  agregarATodos n t

-- Esto lo vi con list comprehensions, pero no se puede usar, así que lo paso a forma recursiva simple
-- Guardo el término anterior y se lo anexo con n agregado a c/elemento. 2^n
-- 0 . [[]]
-- 1. [ [] | [1] ]  <- AgregarATodos 1
-- 2. [ [],[1] | [2],[1,2]  ] <- AgregarATodos 2
-- 3. [ [],[1], [2],[1,2] | [3],[1,3],[2,3],[1,2,3] ] <- AgregarATodos 3
partes 0 = [[]]
partes n = anterior ++ agregarATodos n anterior
           where anterior = partes(n-1)

-------------------------------------------
-- Diapositiva : 12
------------------------------------------

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
cartesianoAux :: Integer -> Set Integer -> Set (Integer, Integer)

-- armo tuples con cada elemento de la lista que nos dan (n,..)
cartesianoAux a [b] = [(a,b)]
cartesianoAux n (h:t) = [(n,h)] ++ (cartesianoAux n t)

-- con list comprehensions sale más lindo
-- [(x,y) | [x,y] <- sequence [a,b]]
productoCartesiano [] _ = []
productoCartesiano (h:t) lista = (cartesianoAux h lista) ++ (productoCartesiano t lista)


insertarEn :: Set Integer -> Integer -> Integer -> Set Integer
alternar :: Integer -> Set Integer -> Set (Set Integer)
variaciones :: Set Integer -> Integer -> Set (Set Integer)
permutaciones :: Set Integer -> Set (Set Integer)
-- bolitasEnCajas :: Integer -> Integer -> Set (Set Integer)


insertarEn (h:t) n 1 = [n,h] ++ t
insertarEn (h:t) n c = h : insertarEn t n (c-1) 

alternar x [] = [[x]]
alternar x (y:ys) = (x:y:ys) : [y:zs | zs <- alternar x ys]

variaciones [] _ = [[]]
variaciones _ 0 = [[]]

variaciones lista n = [y:x | y <- lista, x <- termino ]
                      where termino = variaciones lista (n-1)

permutaciones []     = [[]]
permutaciones (x:xs) = concat [alternar x ys | ys <- permutaciones xs]

-- Llegó el parcial, lol