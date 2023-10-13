module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' s (LVar x) = case elemIndex x s of
                            Just n  -> Bound n
                            Nothing -> Free (Global x)
conversion' s (App t1 t2) = let t1' = conversion' s t1
                                t2' = conversion' s t2
                            in t1' :@: t2'
conversion' s (Abs x t) = Lam $ conversion' (x:s) t

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) v = f v
vapp (VNeutral neu) v = VNeutral $ NApp neu v

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free name) (gEnv, _) = case lookup name gEnv of
                                      Just v -> v
                                      Nothing -> VNeutral $ NFree name
eval' (t1 :@: t2) env = let v1 = eval' t1 env
                            v2 = eval' t2 env
                        in vapp v1 v2
eval' (Lam t) (gEnv, lEnv) = VLam (\v -> eval' t (gEnv, v:lEnv))


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote v = boundVars (quoteValue v 0) 0

quoteValue :: Value -> Int -> Term
quoteValue (VNeutral neu) i = quoteNeutral neu i
quoteValue (VLam f) i = let v = f $ VNeutral $ NFree (Quote i)
                        in Lam $ quoteValue v (i+1)

quoteNeutral :: Neutral -> Int -> Term
quoteNeutral (NFree name) _ = Free name
quoteNeutral (NApp neu v) i = let t1 = quoteNeutral neu i
                                  t2 = quoteValue v i
                              in t1 :@: t2

boundVars :: Term -> Int -> Term
boundVars fr@(Free (Global _)) _ = fr
boundVars (Free (Quote k)) i = Bound (i - k - 1)
boundVars (t1 :@: t2) i = let t1' = boundVars t1 i
                              t2' = boundVars t2 i
                          in t1' :@: t2'
boundVars (Lam t) i = Lam $ boundVars t (i+1)
