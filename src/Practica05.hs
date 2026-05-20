module Practica05 where

import Terminos

--Aplicar una sustitucion a un termino
apsubT :: Term -> Subst -> Term
apsubT a [] = a
apsubT (Var x) ((y, t):ys) =
  if x == y
  then t
  else apsubT (Var x) ys
apsubT (Fun f args) subst = Fun f (aplicarLista args subst)
  
--Funcion auxiliar para aplicar la sustitucion a una lista de terminos
aplicarLista :: [Term] -> Subst -> [Term]
aplicarLista [] _ = []
aplicarLista l [] = l
aplicarLista (x:xs) (y:ys) = (apsubT x (y:ys)):(aplicarLista xs (y:ys))

--Funcion que elimina los pares que son de la forma x=x
simpSus :: Subst -> Subst
simpSus [] = []
simpSus ((x, Var n):xs) =
  if x == n
  then simpSus xs
  else ((x, Var n):(simpSus xs))
simpSus (f:xs) = f:(simpSus xs)

--Funcion que calcula la composicion de dos sustituciones
compSus :: Subst -> Subst -> Subst
compSus s1 s2 = compSusAux(parte1 ++ s2)
  where parte1 = [(x, apsubT t s2) | (x,t) <- s1]

--Funcion auxiliar para compSus
compSusAux :: Subst -> Subst
compSusAux [] = []
compSusAux ((x,t):xs) =
  (x,t):(compSusAux [(y,s) | (y,s) <- xs, y /= x])
  
--Funcion que devuelve un umg de dos terminos, si es que lo hay
unifica :: Term -> Term -> [Subst]
unifica = undefined

--Funcion que devuelve un unificador de dos términos funcionales, si es que lo hay
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas = undefined

--Funcion que devuelve un umg de una lista de termino, si es que lo hay
unificaConj :: [Term] -> [Subst]
unificaConj = undefined

