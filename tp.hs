-- Enunciado 1
-- Calcula el Modelo Exponencial Discreto
-- f(i0, _, 0) = i0
-- f(i0, b, n) = f(i0, b, n-1) + f(i0, b, n-1) * b

med :: Float -> Float -> Int -> Float
med i0 b n | n == 0 = i0
           | otherwise = (med i0 b (n-1)) * (1 + b)

-- Enunciado 2
-- Calcula el Modelo Logistico Discreto
-- b e (0, 1)

mld :: Float -> Float -> Float -> Int -> Float
mld p i0 b n | n == 0 = i0
             | otherwise = infectados * (1 + b * sanosN)
             where infectados = mld p i0 b (n - 1)
                   sanosN = (p - infectados) / p

-- Enunciado 3 y 4
calcInfectados :: (Float, Float, Float) -> Float -> Float -> Int -> Float
calcInfectados (s0, i0, r0) b g n | n == 0 = i0
                                  | otherwise = infectados * (1 + b * sanos - g)
                                  where sanos = calcSanos (s0, i0, r0) b g i
                                        infectados = calcInfectados (s0, i0, r0) b g i
                                        i = n-1

calcSanos :: (Float, Float, Float) -> Float -> Float -> Int -> Float
calcSanos (s0, i0, r0) b g n | n == 0 = s0
                             | otherwise = sanosPrev * (1 - b * infectados)
                             where sanosPrev = calcSanos (s0, i0, r0) b g i
                                   infectados = calcInfectados (s0, i0, r0) b g i
                                   i = n-1

calcRecuperados :: (Float, Float, Float) -> Float -> Float -> Int -> Float
calcRecuperados (s0, i0, r0) b g n | n == 0 = r0
                                   | otherwise = recuperados + g * infectados
                                   where i = n-1
                                         infectados = calcInfectados (s0, i0, r0) b g i
                                         recuperados = calcRecuperados (s0, i0, r0) b g i


sir :: (Float, Float, Float) -> Float -> Float -> Int -> (Float, Float, Float)
sir (s0,i0,r0) b g n = (calcSanos (s0, i0, r0) b g n, calcInfectados (s0, i0, r0) b g n, calcRecuperados (s0, i0, r0) b g n)

takeSecond :: (Float, Float, Float) -> Float
takeSecond (_, p, _) = p

maxsir_ :: (Float, Float, Float, Float, Float) -> Float -> Int -> Int -> Float
maxsir_ (s0, i0, r0, b, g) currentMax i n | i == n = currentMax
                                        | otherwise = maxsir_ (s0, i0, r0, b, g) max (i+1) n
                                        where max = maximum[takeSecond (sir (s0, i0, r0) b g (i+1)), currentMax]

maxsir :: (Float, Float, Float) -> Float -> Float -> Int -> Float
maxsir (s0, i0, r0) b g n = maxsir_ (s0, i0, r0, b, g) (takeSecond (sir (s0, i0, r0) b g 0)) 0 n