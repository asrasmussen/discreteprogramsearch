module Main where

import IntFun
import BinFun

main :: IO ()
main = do 
  putStrLn "Starting"
  putStrLn $ "Result of searching for integer function 2*x     : " ++ show IntFun.firstOk
  putStrLn $ "Result of searching for binary increment function: " ++ show BinFun.firstInc

