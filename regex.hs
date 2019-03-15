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
 matchD = error "Falta Implementar"

 -------------------- DERIVADA ----------------------

 -- EJERCICIO 1
 deriv :: String -> Regex -> Regex
 deriv = error "Falta Implementar"

 -- EJERCICIO 2
 matchV :: String -> Regex -> Bool
 matchV = error "Falta Implementar"


