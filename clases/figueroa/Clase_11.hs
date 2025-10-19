{-- import Modulo --}

{-- | se llaman 'Guards' --}
scoreToLetter :: Int -> Char
scoreToLetter n
  | n > 90 = 'A'
  | n > 80 = 'B'
  | n > 70 = 'C'
  | otherwise = 'F'

{-
(define (scoreToLetter n)
  (cond n
    [(> n 90) 'A]
    [(> n 80) 'B]
    [(> n 70) 'C]
    [else 'F']))
-}

len [] = 0
len (x:s) = 1 + len s

ones = 1 : ones

-- En Haskell se llama "type signature"
-- o firma de la función

front :: Int -> [a] -> [a]
{-- front n l = [] --}
front _ [] = []
front 0 (x:s) = []
front n (x:s) = x : front (n-1) s

{--
 (define (front n l)
   (match (cons n l)
      [(cons n '()) '()]
      [(cons 0 (cons x s)) '()]
      [(cons n (cons x s)) (cons x (front (- n 1) s))]))
--}
--
-- EN los lenguajes con evaluacion temprana, las estructuras
-- infinitas se pueden implementar como "Streams
--
--  EN los lenguajes con evaluacion temprana, las estructuras
--  infinitas se pueden implementar como "Streams".

mycycle1 :: [a] -> [a]
mycycle1 [] = error "Empty list"
mycycle1 xs = xs ++ mycycle1 xs

mycycle2 :: [a] -> [a]
mycycle2 [] = error "Empty list"
mycycle2 (x:xs) = x : mycycle2 (xs ++ [x])


tB :: [String] -> [Int] -> [(String, Int)]
tB [] _ = []
tB (f:fs) (b:bs) = (f,b) : tB fs bs

myzip [] _ = []
myzip _ [] = []
myzip (a:as) (b:bs) = (a,b) : myzip as bs

zipOp :: (a -> b -> c) -> [a] -> [b] -> [c]
zipOp f [] _ = []
zipOp f _ [] = []
zipOp f (a:as) (b:bs) = (f a b) : zipOp f as bs

-- Evaluar a mano para revisar por qué funciona
fibs = 1 : 1 : zipOp (+) fibs (tail fibs)




