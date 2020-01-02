{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A7 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (evalInst,Instruction(..),Stack,SMProg) where

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


data Instruction = Add | Mul | Dup | Pop deriving (Eq,Ord,Show,Generic)
type Stack = [Int]
type SMProg = [Instruction] 

instance NFData (Instruction)

-- Exercise A7
evalInst :: Stack -> SMProg -> Stack

evalInst s p = foldl ops s p 
    where
        ops    []     op = error "No space on stack" 

        ops (x:y:xs) Add = (x+y):xs
        ops    _     Add = error "nothing to add"

        ops  (x:y:xs)Mul = (x*y):xs
        ops     _   Mul = error "nothing to mul"
        
        ops  (x:xs)  Dup = x:x:xs
        ops  (x:xs)  Pop= xs