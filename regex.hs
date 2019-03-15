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
 simpl = error "Falta Implementar"

 -- EJERCICIO 2
 denot :: Regex -> Language
 denot = error "Falta Implementar"

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



