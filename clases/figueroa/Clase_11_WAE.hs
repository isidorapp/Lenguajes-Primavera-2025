-- WAE: With + Arithmetic Expressions

-- Type aliases
type Identifier = String
type Value = Int

-- Ambiente
type Env = [(Identifier, Value)]

-- Sintaxis abstracta
-- 'data' es similar al 'deftype' en #lang play
data WAE = Num Int
         | Add WAE WAE -- definición inductiva
         | Id Identifier
         | With Identifier WAE WAE

interp :: WAE -> Env -> Value
interp (Num n) env = n
interp (Add l r) env = interp l env + interp r env
interp (Id i) env = env_lookup i env
interp (With i named_expr body) env =
       interp body (extend env i (interp named_expr env))

env_lookup :: Identifier -> Env -> Value
env_lookup _ [] = error "Free identifier"
env_lookup i ((x,v):r)
   | (i == x) = v
   | otherwise = env_lookup i r

extend :: Env -> Identifier -> Value -> Env
extend env i v = (i,v) : env

{-
 - Si extendemos el interprete con funciones 
 - (utilizando funciones de Haskell para representar funciones del lenguaje),
 - qué semántica de evaluación tendrán las funciones en el lenguaje interpretado?
-}




