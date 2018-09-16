module Clase01 (f, doble) where

-- Clase 1: 
f :: Float -> Float -> Float
g :: Float -> Float -> Float -> Float

doble :: Integer -> Integer
triple :: Integer -> Integer
suma :: Integer -> Integer -> Integer

normaVectorial :: Float -> Float -> Float
functionConstante8 :: Integer -> Integer

-- Nuestras primeras funciones en Haskell !

f x y = x ^ 2 + y ^ 2
g x y z = x + y + z * z

doble x = 2 * x
triple x = 3 * x

suma x y = x + y

normaVectorial x y = sqrt (f x y)

functionConstante8 x = 8
respuestaATodo = 42
