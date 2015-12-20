{- |
Module      : $Header$
Description : REIL basic block
Maintainer  : Adrian Herrera
Stability   : experimental
-}

{-# LANGUAGE ViewPatterns #-}

module Data.REIL.BasicBlock (
    -- * Statement
    Statement(..),

    -- * Basic block
    BasicBlock(..),
    empty,
    null,
    startAddress,
    endAddress,
    addStatement,
) where

import Prelude hiding (null)
import qualified Data.Map as M

import qualified Data.REIL.InstructionSet as IS

-------------------------------------------------------------------------------
-- Statement
-------------------------------------------------------------------------------

-- | A statement consists of an instruction at a specific address
data Statement =
    Statement IS.Address IS.Instruction

instance Show Statement where
    show (Statement addr inst) =
        IS.showAddress addr ++ ": " ++ show inst

-- Find an address in a statement map based on a given `find` function (e.g.
-- Map.findMin or Map.findMax)
findAddr :: (M.Map IS.Address IS.Instruction -> (IS.Address, IS.Instruction))
            -> M.Map IS.Address IS.Instruction
            -> Maybe IS.Address
findAddr _ (M.null -> True) =
    Nothing
findAddr func stmts =
    Just $ fst $ func stmts

-- Find the minimum address in a statement map
findMinAddr :: M.Map IS.Address IS.Instruction -> Maybe IS.Address
findMinAddr =
    findAddr M.findMin

-- Find the maximum address in a statement map
findMaxAddr :: M.Map IS.Address IS.Instruction -> Maybe IS.Address
findMaxAddr =
    findAddr M.findMax

-------------------------------------------------------------------------------
-- Basic block
-------------------------------------------------------------------------------

-- | A basic block consists of a map of statements
newtype BasicBlock =
    BasicBlock (M.Map IS.Address IS.Instruction)

-- | Create an empty basic block
empty :: BasicBlock
empty =
    BasicBlock M.empty

-- | Check if the basic block is empty
null :: BasicBlock -> Bool
null (BasicBlock stmts) =
    M.null stmts

-- | Get the start address for a basic block. If the basic block is empty,
-- return @Nothing@
startAddress :: BasicBlock -> Maybe IS.Address
startAddress (BasicBlock stmts) =
    findMinAddr stmts

-- | Get the end address for a basic block. If the basic block is empty, return
-- @Nothing@
endAddress :: BasicBlock -> Maybe IS.Address
endAddress (BasicBlock stmts) =
    findMaxAddr stmts

-- | Add a statement to the end of a basic block. The statement is only added
-- if its address is greater than the address of the last statement in the
-- basic block
addStatement :: BasicBlock -> Statement -> BasicBlock
addStatement (null -> True) (Statement addr inst) =
    BasicBlock $ M.singleton addr inst
addStatement (BasicBlock stmts) (Statement addr inst)
    | checkStatement = BasicBlock $ M.insert addr inst stmts
    | otherwise = error "Unable to add the statement to the basic block"
    -- Check that the address of the instruction that we are about to add to
    -- the basic block does not already exist.
    --
    -- Also check that the address of the instruction that we are about to add
    -- to the basic block is greater than the last statement in the basic block
    where checkStatement = M.notMember addr stmts &&
                           addr > (fst $ M.findMax stmts)
