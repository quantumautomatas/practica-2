module Regex where

 import Data.List

 -- Tipo de dato algebraico para representar Expresiones Regulares
 data Regex = Void
            | Epsilon
            | Symbol Char           -- El símbolo representado por el caracter que recibe
            | Star Regex            -- r*
            | Concat Regex Regex    -- (rs)
            | Add Regex Regex       -- (r + s)
            deriving (Eq)

 -- Sinónimo para representar lenguajes como listas de cadenas.
 type Language = [String]

 -- Instancia de Show del tipo Regex, para que se impriman con formato en la consola.
 instance Show Regex where
   show Void = "ø"
   show Epsilon = "ε"
   show (Symbol c) = c:[]
   show (Star r) = show r ++ "*"
   show (Concat r s) = "(" ++ show r ++ show s ++ ")"
   show (Add r s) = "(" ++ show r ++ " + " ++ show s ++ ")"

  ------------------- DENOTACIÓN -----------------------

 -- EJERCICIO 1
 simpl :: Regex -> Regex
 simpl r = simplN r 10000

 simplN :: Regex -> Int -> Regex
 simplN r 0 = r
 simplN r n = simplN (simplAux r) (n-1)

 simplAux :: Regex -> Regex
 simplAux Void = Void
 simplAux Epsilon = Epsilon
 simplAux (Symbol c) = Symbol c
 simplAux (Star (Add Epsilon a)) = Star (simplAux a)
 simplAux (Star Epsilon) = Epsilon
 simplAux (Star Void) = Epsilon
 simplAux (Star (Star r)) = Star (simplAux r)
 simplAux (Star r) = Star (simplAux r)
 simplAux (Concat _ Void) = Void
 simplAux (Concat Void _) = Void
 simplAux (Concat a Epsilon) = simplAux a
 simplAux (Concat Epsilon a) = simplAux a
 simplAux (Concat (Star r)(Star s)) = 
  if r == s 
    then Star (simplAux r)
    else Concat (Star (simplAux r))(Star (simplAux s))
 simplAux (Concat r s) = Concat (simplAux r) (simplAux s)
 simplAux (Add a Void) = simplAux a
 simplAux (Add Void a) = simplAux a
 simplAux (Add Epsilon (Concat (Star a) b)) = 
  if a == b 
    then Star (simplAux a)
    else (Add (Epsilon) (Concat (Star (simplAux a)) (simplAux b)))
 simplAux (Add r s) = 
  if r == s 
    then simplAux r
    else (Add (simplAux r) (simplAux s))

 -- EJERCICIO 2
 denot :: Regex -> Language
 denot Void = []
 denot Epsilon = [""]
 denot (Symbol c) = [[c]]
 denot (Star r) = denotStar r
 denot (Concat r1 r2) = concatLen (denot r1) (denot r2)
 denot (Add r1 r2) = union (denot r1) (denot r2)

 denotStar :: Regex -> Language
 denotStar r = "" : (l ++ (concatLen l (denot (Star r)))) where l = denot r

 concatLen :: Language -> Language -> Language
 concatLen _ [] = []
 concatLen a (x:xs) = [y++x | y <- a] ++ (concatLen a xs)

 -- EJERCICIO 3
 matchD :: String -> Regex -> Bool
 matchD s r= elem s (denot r)

 -------------------- DERIVADA ----------------------

 -- EJERCICIO 1
 deriv :: String -> Regex -> Regex
 deriv [] r = simpl r
 deriv (x:xs) r = simpl (deriv xs (d x r))

  -- Derivada respecto a un símbolo
 d :: Char -> Regex -> Regex
 d _ Void = Void
 d _ Epsilon = Void
 d x (Symbol y) =
    if x == y
        then Epsilon
        else Void
 d x (Star y) = Concat (d x y) (Star y)
 d x (Concat y z) = Add (Concat (d x y) z) (Concat (nul y) (d x z))
 d x (Add y z) =  Add (d x y) (d x z)

  -- función de nulidad
 nul :: Regex -> Regex
 nul Void = Void
 nul Epsilon = Epsilon
 nul (Symbol _) = Void
 nul (Star _) = Epsilon
 nul (Concat  x y) = Concat (nul x) (nul y)
 nul (Add x y ) = Add (nul x) (nul y)

  -- EJERCICIO 2
 matchV :: String -> Regex -> Bool
 matchV s r = simpl (nul (deriv s r)) == Epsilon
