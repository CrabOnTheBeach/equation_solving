--f(x) is left part of the equation f(x) = 0
f :: Double -> Double
f x = 2 * (log x) - 1 / x
f' x = 2 / x + 1 / x^2
f'' x = -2 * (1 / x^2 + 1 / x^3)
precision = (1/10)^(7)

--Implements Newton method for case f(a) * f''(a) > 0
newton :: (Double -> Double) -> Double -> Double -> (Double, Integer)
newton fn x e =
    helper (fn) 1000 x 0 e
    where 
        helper (fn) x0 x1 count e
            | abs(x0 - x1) < e = (x1, count)
            | otherwise = helper (fn) (x1) (x1 - f(x1) / f'(x1)) (count + 1) (e)

--Implements chord method
chord :: (Double->Double) -> Double -> Double -> Double -> (Double, Integer)
chord fn a b e =
    helper (fn) 1000 x a b 0 e
    where 
        helper (fn) x0 x1 a b count e
            | abs(x0 - x1) < e = (x1, count)
            | otherwise = helper (fn) (x1) (x1 - f(x1) * (x1 - c) / (f(x1) - f(c))) (a) (b) (count + 1) (e)
            where
                c = if f a * f'' a > 0 then a else b
        x = if f a * f'' a < 0 then a else b

--Implements secant method
secant :: (Double->Double) -> Double -> Double -> Double -> (Double, Integer)
secant fn a b e =
    helper (fn) a b 0 e
    where
        helper (fn) x1 x2 count e
            | abs(x1 - x2) < e = (x2, count)
            | otherwise = helper (fn) (x2) (x2 - f(x2) / (f(x2) - f(x1)) * (x2 - x1)) (count + 1) e

--Implenents finite difference Newton method
newton2 :: (Double->Double) -> Double -> Double -> Double -> (Double, Integer)
newton2 fn a b e =
    helper (fn) 1000 a 0 e
    where
        helper (fn) x1 x2 count e
            | abs(x1 - x2) < e = (x2, count)
            | otherwise = helper (fn) (x2) (x2 - h * f(x2) / (f(x2 + h) - f(x2))) (count + 1) e
            where h = 0.05

--Implements Steffensen method
steffensen :: (Double->Double) -> Double -> Double -> Double -> (Double, Integer)
steffensen fn a b e =
    helper (fn) 1000 (a + 0.1) 0 e
    where
        helper (fn) x1 x2 count e
            | abs(x1 - x2) < e = (x2, count)
            | otherwise = helper (fn) (x2) (x2 - h * f(x2) / (f(x2 + h) - f(x2))) (count + 1) e
            where h = f(x2)

--Implements simple iterations method
simple_iterations :: (Double->Double) -> Double -> Double -> Double -> (Double, Integer)
simple_iterations fn a b e =
    helper (fn) 1000 (a + 0.1) 0 e
    where
        helper (fn) x1 x2 count e
            | abs(x1 - x2) < e = (x2, count)
            | otherwise = helper (fn) (x2) (x2 - t * f(x2)) (count + 1) e
            where t = 1 / f'(a)

main = do
    print (newton f 1 precision, "Newton")
    print (chord f 1 2 precision, "Chord")
    print (secant f 1 2 precision, "Secant")
    print (newton2 f 1 2 precision, "Newton2")
    print (steffensen f 1 2 precision,"Steffensen")
    print (simple_iterations f 1 2 precision, "Simple iterations")