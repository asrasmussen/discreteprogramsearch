module IntFun where

import Control.Monad
import Control.Monad.Omega
import Control.Applicative ((<|>))

data Op = Mult | Plus | Sub | Div deriving (Show)

data Term =
  Var
  | ConstNum Int
  | Binary Op Term Term
  deriving (Show)


terms :: Omega Term
terms = 
  return Var 
  <|> each (map ConstNum [0..]) 
  <|> liftM2 (Binary Mult) terms terms 
  <|> liftM2 (Binary Div) terms terms
  <|> liftM2 (Binary Plus) terms terms
  <|> liftM2 (Binary Sub) terms terms


eval :: Int -> Term -> Int
eval _ (ConstNum x) = x
eval input Var = input
eval input (Binary Mult t1 t2) = (eval input t1) * (eval input t2)
eval input (Binary Div t1 t2) = (eval input t1) `div` (eval input t2)
eval input (Binary Plus t1 t2) = (eval input t1) + (eval input t2)
eval input (Binary Sub t1 t2)  = (eval input t1) - (eval input t2)


constraints :: [(Int, Int)]
constraints = [(1, 2), (2, 4), (4, 8)]

checkConstraints :: Term -> Bool
checkConstraints term = all id $ map (\t -> eval (fst t) term == (snd t)) constraints


firstOk :: (Int, Term)
firstOk = head $ filter (\pair -> checkConstraints (snd pair)) $ zip [1..] (runOmega terms)

