{- |
Module      : $Header$
Description : REIL basic block
Maintainer  : Adrian Herrera
Stability   : experimental
-}

{-# LANGUAGE ViewPatterns #-}

module Data.REIL.BasicBlock (
    Stmt(..),
    BasicBlock(..),
    emptyBasicBlock,
    startAddress,
    endAddress,
    addStmt,
) where

import Data.Sequence as S
import Data.Foldable (toList)

import qualified Data.REIL.InstructionSet as IS

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

-- | First element of a sequence, or @Nothing@ if the sequence is empty
seqHead :: Seq a -> Maybe a
seqHead (viewl -> x :< _) =
    Just x
seqHead _ =
    Nothing

-- | Last element of a sequence, or @Nothing@ if the sequence is empty
seqLast :: Seq a -> Maybe a
seqLast (viewr -> _ :> x) =
    Just x
seqLast _ =
    Nothing

-------------------------------------------------------------------------------

-- | A statement consists of an instruction at a specific address
data Stmt =
    Stmt {
        -- | The Address that the instruction is located at
        stmtAddr :: IS.Address,
        -- | The statement's instruction
        stmtInst :: IS.Instruction
    }

instance Show Stmt where
    show (Stmt addr inst) =
        IS.showAddress addr ++ ": " ++ show inst

-- | A basic block consists of a start address and a list of statements
-- contained within
newtype BasicBlock =
    BasicBlock (Seq Stmt)

instance Show BasicBlock where
    show (BasicBlock stmts) =
        show $ toList stmts

-- | Get the start address for a basic block. If the basic block is empty,
-- return @Nothing@
startAddress :: BasicBlock -> Maybe IS.Address
startAddress (BasicBlock (seqHead -> Just stmt)) =
    Just $ stmtAddr stmt
startAddress (BasicBlock _) =
    Nothing

-- | Get the end address for a basic block. If the basic block is empty, return
-- @Nothing@
endAddress :: BasicBlock -> Maybe IS.Address
endAddress (BasicBlock (seqLast -> Just stmt)) =
    Just $ stmtAddr stmt
endAddress (BasicBlock _) =
    Nothing

-- | Create an empty basic block
emptyBasicBlock :: BasicBlock
emptyBasicBlock =
    BasicBlock empty

-- | Add a statement to a basic block. The statement is only added if its
-- address is greater than the address of the last statement in the basic block
addStmt :: BasicBlock -> Stmt -> BasicBlock
addStmt (BasicBlock stmts) stmt
    | checkLastStmt = BasicBlock $ stmts |> stmt
    | otherwise = error ""
    -- Check that the statement that we are about to add to the basic block
    -- has an address greater than that of the last statement in this basic
    -- block
    where checkLastStmt = case seqLast stmts of
                            Nothing -> True
                            Just lastStmt -> stmtAddr stmt > stmtAddr lastStmt
