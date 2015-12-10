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
    empty,
    startAddress,
    endAddress,
    addStmt,
) where

import qualified Data.Sequence as S
import Data.Foldable (toList)

import qualified Data.REIL.InstructionSet as IS

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

-- | First element of a sequence, or @Nothing@ if the sequence is empty
seqHead :: S.Seq a -> Maybe a
seqHead (S.viewl -> x S.:< _) =
    Just x
seqHead _ =
    Nothing

-- | Last element of a sequence, or @Nothing@ if the sequence is empty
seqLast :: S.Seq a -> Maybe a
seqLast (S.viewr -> _ S.:> x) =
    Just x
seqLast _ =
    Nothing

-------------------------------------------------------------------------------
-- Statement
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

-------------------------------------------------------------------------------
-- Basic block
-------------------------------------------------------------------------------

-- | A basic block consists of a sequence of statements
newtype BasicBlock =
    BasicBlock (S.Seq Stmt)

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
empty :: BasicBlock
empty =
    BasicBlock S.empty

-- | Add a statement to the end of a basic block. The statement is only added
-- if its address is greater than the address of the last statement in the
-- basic block
addStmt :: BasicBlock -> Stmt -> BasicBlock
addStmt (BasicBlock stmts) stmt
    | checkLastStmt = BasicBlock $ stmts S.|> stmt
    | otherwise = error "Unable to add the statement to the basic block"
    -- Check that the statement that we are about to add to the basic block
    -- has an address greater than that of the last statement in this basic
    -- block
    where checkLastStmt = case seqLast stmts of
                            Nothing -> True
                            Just lastStmt -> stmtAddr stmt > stmtAddr lastStmt
