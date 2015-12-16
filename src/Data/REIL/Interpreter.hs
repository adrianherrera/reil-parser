{- |
Module      : $Header$
Description : REIL interpreter
Maintainer  : Adrian Herrera
Stability   : experimental
-}

module Data.REIL.Interpreter (
) where

import qualified Data.Map as M

import qualified Data.REIL.InstructionSet as IS

-- | The environment describes a program's "state". It consists of data stored
-- in both registers and memory.
data Environment a =
    Environment {
        -- | Map of register names to the value the register contains
        registers :: M.Map IS.RegisterName a,
        -- | Map of addresses to the value stored at that address
        memory :: M.Map IS.Address a
    }

-- | An interpreter "executes" instructions and updates its environment as a
-- result
class Interpreter a where
    -- | Given an environment, "execute" an instruction and return a new,
    -- updated environment. This function should describe the language's big
    -- step operational semantics
    execute :: IS.Instruction -> Environment a -> Environment a

-- | This interpreter instance essentially describes REIL's concrete semantics
instance Interpreter Int where
    -- Add instruction
    execute (IS.Add (IS.IntegerLiteral i1 _)
                      (IS.IntegerLiteral i2 _)
                      (IS.Register r _)) env =
        undefined
    execute (IS.Add (IS.IntegerLiteral i _)
                      (IS.Register r1 _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Add (IS.Register r1 _)
                      (IS.IntegerLiteral i _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Add (IS.Register r1 _)
                      (IS.Register r2 _)
                      (IS.Register r3 _)) env =
        undefined
    -- And instruction
    execute (IS.And (IS.IntegerLiteral i1 _)
                      (IS.IntegerLiteral i2 _)
                      (IS.Register r _)) env =
        undefined
    execute (IS.And (IS.IntegerLiteral i _)
                      (IS.Register r1 _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.And (IS.Register r1 _)
                      (IS.IntegerLiteral i _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.And (IS.Register r1 _)
                      (IS.Register r2 _)
                      (IS.Register r3 _)) env =
        undefined
    -- Bisz instruction
    execute (IS.Bisz (IS.IntegerLiteral i _)
                       _
                       (IS.Register r _)) env =
        undefined
    execute (IS.Bisz (IS.Register r1 _)
                       _
                       (IS.Register r2 _)) env =
        undefined
    -- Bsh instruction
    execute (IS.Bsh (IS.IntegerLiteral i1 _)
                      (IS.IntegerLiteral i2 _)
                      (IS.Register r _)) env =
        undefined
    execute (IS.Bsh (IS.IntegerLiteral i1 _)
                      (IS.Register r1 _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Bsh (IS.Register r1 _)
                      (IS.IntegerLiteral i _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Bsh (IS.Register r1 _)
                      (IS.Register r2 _)
                      (IS.Register r3 _)) env =
        undefined
    -- Div instruction
    execute (IS.Div (IS.IntegerLiteral i1 _)
                      (IS.IntegerLiteral i2 _)
                      (IS.Register r _)) env =
        undefined
    execute (IS.Div (IS.IntegerLiteral i _)
                      (IS.Register r1 _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Div (IS.Register r1 _)
                      (IS.IntegerLiteral i _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Div (IS.Register r1 _)
                      (IS.Register r2 _)
                      (IS.Register r3 _)) env =
        undefined
    -- Jcc instruction
    execute (IS.Jcc (IS.IntegerLiteral i1 _)
                      _
                      (IS.IntegerLiteral i2 _)) env =
        undefined
    execute (IS.Jcc (IS.IntegerLiteral i _)
                      _
                      (IS.Register r _)) env =
        undefined
    execute (IS.Jcc (IS.IntegerLiteral i _)
                      _
                      (IS.Offset o)) env =
        undefined
    execute (IS.Jcc (IS.Register r _)
                      _
                      (IS.IntegerLiteral i _)) env =
        undefined
    execute (IS.Jcc (IS.Register r1 _)
                      _
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Jcc (IS.Register r _)
                      _
                      (IS.Offset o)) env =
        undefined
    -- Ldm instruction
    execute (IS.Ldm (IS.IntegerLiteral i _)
                      _
                      (IS.Register r _)) env =
        undefined
    execute (IS.Ldm (IS.Register r1 _)
                      _
                      (IS.Register r2 _)) env =
        undefined
    -- Mod instruction
    execute (IS.Mod (IS.IntegerLiteral i1 _)
                      (IS.IntegerLiteral i2 _)
                      (IS.Register r _)) env =
        undefined
    execute (IS.Mod (IS.IntegerLiteral i _)
                      (IS.Register r1 _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Mod (IS.Register r1 _)
                      (IS.IntegerLiteral i _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Mod (IS.Register r1 _)
                      (IS.Register r2 _)
                      (IS.Register r3 _)) env =
        undefined
    -- Mul instruction
    execute (IS.Mul (IS.IntegerLiteral i1 _)
                      (IS.IntegerLiteral i2 _)
                      (IS.Register r _)) env =
        undefined
    execute (IS.Mul (IS.IntegerLiteral i _)
                      (IS.Register r1 _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Mul (IS.Register r1 _)
                      (IS.IntegerLiteral i _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Mul (IS.Register r1 _)
                      (IS.Register r2 _)
                      (IS.Register r3 _)) env =
        undefined
    -- Nop instruction
    execute (IS.Nop _ _ _) env =
        env
    -- Or instruction
    execute (IS.Or (IS.IntegerLiteral i1 _)
                      (IS.IntegerLiteral i2 _)
                      (IS.Register r _)) env =
        undefined
    execute (IS.Or (IS.IntegerLiteral i _)
                      (IS.Register r1 _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Or (IS.Register r1 _)
                      (IS.IntegerLiteral i _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Or (IS.Register r1 _)
                      (IS.Register r2 _)
                      (IS.Register r3 _)) env =
        undefined
    -- Stm instruction
    execute (IS.Stm (IS.IntegerLiteral i1 _)
                      _
                      (IS.IntegerLiteral i2 _)) env =
        undefined
    execute (IS.Stm (IS.IntegerLiteral i _)
                      _
                      (IS.Register r _)) env =
        undefined
    execute (IS.Stm (IS.Register r _)
                      _
                      (IS.IntegerLiteral i _)) env =
        undefined
    execute (IS.Stm (IS.Register r1 _)
                   _
                   (IS.Register r2 _)) env =
        undefined
    -- Str instruction
    execute (IS.Str (IS.IntegerLiteral i _)
                      _
                      (IS.Register r _)) env =
        undefined
    execute (IS.Str (IS.Register r1 _)
                      _
                      (IS.Register r2 _)) env =
        undefined
    -- Sub instruction
    execute (IS.Sub (IS.IntegerLiteral i1 _)
                      (IS.IntegerLiteral i2 _)
                      (IS.Register r _)) env =
        undefined
    execute (IS.Sub (IS.IntegerLiteral i _)
                      (IS.Register r1 _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Sub (IS.Register r1 _)
                      (IS.IntegerLiteral i _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Sub (IS.Register r1 _)
                      (IS.Register r2 _)
                      (IS.Register r3 _)) env =
        undefined
    -- Undef instruction
    execute (IS.Undef _ _ (IS.Register r _)) env =
        undefined
    -- Unkn instruction
    execute (IS.Unkn _ _ _) env =
        env
    -- Xor instruction
    execute (IS.Xor (IS.IntegerLiteral i1 _)
                      (IS.IntegerLiteral i2 _)
                      (IS.Register r _)) env =
        undefined
    execute (IS.Xor (IS.IntegerLiteral i _)
                      (IS.Register r1 _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Xor (IS.Register r1 _)
                      (IS.IntegerLiteral i _)
                      (IS.Register r2 _)) env =
        undefined
    execute (IS.Xor (IS.Register r1 _)
                      (IS.Register r2 _)
                      (IS.Register r3 _)) env =
        undefined
    -- Error
    execute inst _ =
        error $ "Invalid instruction: " ++ show inst
