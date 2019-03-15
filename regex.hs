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
 simpl Void = Void
 simpl Epsilon = Epsilon
 simpl (Symbol c) = Symbol c
 simpl (Star (Add Epsilon a)) = Star (simpl a)
 simpl (Star Epsilon) = Epsilon
 simpl (Star Void) = Epsilon
 simpl (Star (Star r)) = Star (simpl r)
 simpl (Star r) = Star (simpl r)
 simpl (Concat _ Void) = Void
 simpl (Concat Void _) = Void
 simpl (Concat a Epsilon) = simpl a
 simpl (Concat Epsilon a) = simpl a
 simpl (Concat (Star r)(Star s)) = 
  if r == s 
    then Star (simpl r)
    else Concat (Star (simpl r))(Star (simpl s))
 simpl (Concat r s) = Concat (simpl r) (simpl s)
 simpl (Add a Void) = simpl a
 simpl (Add Void a) = simpl a
 simpl (Add Epsilon (Concat (Star a) b)) = 
  if a == b 
    then Star (simpl a)
    else (Add (Epsilon) (Concat (Star (simpl a)) (simpl b)))
 simpl (Add r s) = 
  if r == s 
    then simpl r
    else (Add (simpl r) (simpl s))

 -- EJERCICIO 2
 denot :: Regex -> Language
 denot Void = []
 denot Epsilon = [""]
 denot (Symbol c) = [[c]]
 denot (Star r) = denotStar r
 denot (Concat r1 r2) = concatLen (denot r1) (denot r2)
 denot (Add r1 r2) = union (denot r1) (denot r2)

 denotStar :: Regex -> Language
 denotStar (Star r) = "" : ((denot r) ++ (concatLen (denot r) (denot (Star r))))
 denotStar r = denot r

 concatLen :: Language -> Language -> Language
 concatLen _ [] = []
 concatLen a (x:xs) = [y++x | y <- a] ++ (concatLen a xs)

 -- EJERCICIO 3
 matchD :: String -> Regex -> Bool
 matchD s r= elem s (denot r)

 -------------------- DERIVADA ----------------------

 -- EJERCICIO 1
 deriv :: String -> Regex -> Regex
 deriv [] r = r
 deriv (x:xs) r = deriv xs (d x r)

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
 matchV s r = buscaEps (deriv s r)

  -- aux
 buscaEps :: Regex -> Bool
 buscaEps Void = False
 buscaEps Epsilon = True
 buscaEps (Symbol _) = False
 buscaEps (Star _) = True
 buscaEps (Concat r1 r2) = (buscaEps r1) && (buscaEps r2)
 buscaEps (Add r1 r2) = (buscaEps r1) || (buscaEps r2)
