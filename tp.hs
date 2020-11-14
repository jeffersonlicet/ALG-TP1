{-
  Ejercicio 1
  Calcula el Modelo Exponencial Discreto
-}
med :: Float -> Float -> Int -> Float
med i0 b n | n == 0 = i0
           | otherwise = med i0 b (n-1) * (1 + b)

{-
  Ejercicio 2
  Calcula el Modelo Logistico Discreto
-}
mld :: Float -> Float -> Float -> Int -> Float
mld p i0 b n
  | n == 0 = i0
  | otherwise = infectados * (1 + b * sanosAnteriores)
  where infectados = mld p i0 b (n - 1)
        sanosAnteriores = (p - infectados) / p

{-
  Ejercicio 3
  Calcula el Modelo SIR Discreto
-}
sir :: (Float, Float, Float) -> Float -> Float -> Int -> (Float, Float, Float)
sir (s0, i0, r0) _ _ 0 = (s0, i0, r0)
sir (s0, i0, r0) b g n = (sanos, infectados, recuperados)
  where (sanosPrevios, infectadosPrevios, recuperadosPrevios) = sir (s0, i0, r0) b g (n-1)
        sanos = sanosPrevios * (1 - b * infectadosPrevios)
        recuperados = recuperadosPrevios + g * infectadosPrevios
        infectados = infectadosPrevios * (1 + b * sanosPrevios - g)

{-
  Ejercicio 4
  Calcula el Modelo SIR y retorna el maximo de infectados en n dias
-}
maxsir :: (Float, Float, Float) -> Float -> Float -> Int -> Float
maxsir (s0, i0, r0) b g n = maxSir' (s0, i0, r0) b g n (takeSecond sirN)
  where sirN = sir (s0, i0, r0) b g n

{-
  Funciones auxiliares
-}

{-
-- maxSir'
-- Hace recursion desde n hasta 0 calculando sir(n) y conservando el máximo
-}
maxSir' :: (Float, Float, Float) -> Float -> Float -> Int -> Float -> Float
maxSir' (s0, i0, r0) b g n max'
  | n == 0 = max'
  | otherwise = maxSir' (s0, i0, r0) b g (n-1) nuevoMax
  where sirAnterior = sir (s0, i0, r0) b g (n-1)
        nuevoMax = maximo (takeSecond sirAnterior) max'

{-
-- takeSecond
-- Funcion de ayuda para deconstruir el vector de SIR
-- Obtiene la segunda entrada de un vector de tres elementos
-}
takeSecond :: (Float, Float, Float) -> Float
takeSecond (_, p, _) = p


{-
-- maximo
-- Funcion de ayuda para obtener el maximo entre dos Floats
-}
maximo :: Float -> Float -> Float
maximo a b | a >= b = a
           | otherwise = b