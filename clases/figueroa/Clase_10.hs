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

