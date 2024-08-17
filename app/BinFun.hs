module BinFun where

import Control.Monad
import Control.Monad.Omega
import Control.Applicative ((<|>))
import Data.Bits

-- We want to model a function that takes a binary string as input and outputs a binary string
data BitString =
  Empty
  | Zero BitString
  | One BitString
  deriving (Show)

data Term =
 EmitEmpty
 | EmitZero Term
 | EmitOne Term
 | If Term Term
 | Recurse
 | Return
 deriving (Show)


allSeqs :: Omega BitString 
allSeqs = return Empty <|> liftM Zero allSeqs <|> liftM One allSeqs 

nonEmpty :: Omega BitString 
nonEmpty = liftM Zero allSeqs <|> liftM One allSeqs 

terms :: Omega Term
terms = return Return <|> return EmitEmpty <|> liftM EmitZero terms <|> liftM EmitOne terms <|> liftM2 If terms terms <|> return Recurse


-- Take input value, convert to binary string, evaluate function, convert result to integer
eval :: Int -> Term -> Maybe Int
eval input body = binToInt . (eval_ 10 body body) . intToBin $ input

binToInt :: BitString -> Maybe Int
binToInt = binToInt_ 0 0 False
  where
    binToInt_ :: Int -> Int -> Bool -> BitString -> Maybe Int
    binToInt_ _ _ False Empty = Nothing
    binToInt_ _ acc True Empty = Just acc
    binToInt_ level acc _ (Zero term) = binToInt_ (level + 1) (acc) True term
    binToInt_ level acc _ (One term) = binToInt_ (level + 1) (acc + 2^level) True term

intToBin :: Int -> BitString 
intToBin 0 = Zero Empty
intToBin x = intToBin_ nbits x Empty
  where

    nbits = ( (floor $ (log $ fromIntegral x :: Double) / (log 2.0)) :: Int)
    intToBin_ :: Int -> Int -> BitString -> BitString
    intToBin_ (-1) _ acc = acc
    intToBin_ bitindex y acc 
      | (y `shiftR` bitindex) `mod` 2 == 1 = intToBin_ (bitindex - 1) y (One acc)
      | otherwise = intToBin_ (bitindex - 1) y (Zero acc)

eval_ :: Int -> Term -> Term -> BitString -> BitString
eval_ _ _ (EmitEmpty) _ = Empty
eval_ recDepth orig (EmitZero next) x = Zero $ eval_ recDepth orig next x
eval_ recDepth orig (EmitOne next)  x = One  $ eval_ recDepth orig next x
eval_ _ _ Return x = x
eval_ recDepth orig (If _true false) (Zero rest) = eval_ recDepth orig false rest
eval_ recDepth orig (If true _false) (One rest) = eval_ recDepth orig true rest
eval_ recDepth orig (If _true false) Empty = eval_ recDepth orig false Empty 
eval_ 0 _ Recurse _ = Empty
eval_ recDepth orig Recurse x = eval_ (recDepth - 1) orig orig x


constraints :: [(Int, Int)]
constraints = [(0, 1), (1, 2), (2, 3), (3, 4)]
-- constraints = [(0, 0), (1, 1), (3, 3), (128, 128)]
-- constraints = [(0, 0), (1, 0), (3, 0)]


checkAllConstraints :: Term -> Bool
checkAllConstraints term = all id $ map (evalConstraint term) constraints
  where
    evalConstraint term' (input, output) = case eval input term' of 
                                           Just x -> x == output
                                           Nothing -> False

firstInc :: (Int, Term)
firstInc = head $ filter (\pair -> checkAllConstraints (snd pair)) $ zip [1..] (runOmega terms)
