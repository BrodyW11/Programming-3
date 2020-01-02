{-# LANGUAGE DeriveGeneric #-}

--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2019

--EXERCISE A6 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

module Exercises (insertFromCurrentNode,VTree(..),Direction(..),Trail(..),Zipper(..)) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq 

data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving (Eq,Show,Generic,Generic1)
type Trail a = [Direction a]
type Zipper a = (VTree a, Trail a)

instance NFData a => NFData (VTree a)
instance NFData a => NFData (Direction a)


-- Exercise A6
insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a

goUp   (node , L a visited right : rest) = (Node node a (visited+1) right , rest)
goUp   (node , R a visited  left : rest) = (Node left a (visited+1) node , rest)
goLeft ( Node (Node l a visited r) pa pvis pr ,
         trail) 
       = (Node l a (visited+1) r , L pa pvis pr : trail)
goLeft (Node Leaf pa pvis pr, trail) = (Leaf, L pa pvis pr : trail)

goRight ( Node pl pa pvis (Node l a visited r) ,
          trail) 
       = (Node l a (visited+1) r , R pa pvis pl : trail)
goRight (Node pl pa pvis Leaf, trail) = (Leaf, R pa pvis pl : trail)

getNum (Node _ a _ _) = a

goUpLarge num z@(node, trail)
     | null trail         = z
     | num <= getNum node = z
     | otherwise          = goUpLarge num (goUp z)
goUpSmall num z@(node, trail)
     | null trail         = z
     | num >= getNum node = z
     | otherwise          = goUpSmall num (goUp z)

insertDown num z@(node, trail)
     | node == Leaf        = (Node Leaf num 1 Leaf, trail)
     | num > getNum node   =  insertDown num (goRight z)
     | num < getNum node   =  insertDown num (goLeft z)
     | otherwise           =  z
  
insertFromCurrentNode num z@(node, trail)
     | node == Leaf       = (Node Leaf num 1 Leaf, trail) 
     | num > getNum node  =  insertDown num (goUpLarge num z)
     | num < getNum node  =  insertDown num (goUpSmall num z)
     | otherwise          =  z