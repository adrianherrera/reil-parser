{- |
Module      : $Header$
Description : REIL interpreter and its environment
Maintainer  : Adrian Herrera
Stability   : experimental
-}

module Data.REIL.Interpreter (
    -- * Environment
    Environment(..),
    newEnvironment,
    readRegister,
    writeRegister,
    readMemory,
    writeMemory,

    -- * Interpreter
    Interpreter(..),
) where

import Data.Bits
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

-- | Create a new, empty environment
newEnvironment :: Environment a
newEnvironment =
    Environment {
        registers = M.empty,
        memory = M.empty
    }

-- | Read a register's value. Calls 'error' if the register is undefined (see
-- the @undef@ instruction).
readRegister :: IS.RegisterName -> Environment a -> a
readRegister reg env =
    case M.lookup reg (registers env) of
        Just val -> val
        Nothing -> error $ "Register " ++ show reg ++ " is not defined"

-- | Write a value to a register
writeRegister :: IS.RegisterName -> a -> Environment a -> Environment a
writeRegister reg val env =
    Environment {
        registers = M.insert reg val (registers env),
        memory = memory env
    }

-- | Read a value stored at a memory address, or a default value if it hasn't
-- previously been set
readMemory :: IS.Address -> a -> Environment a -> a
readMemory addr dflt env =
    case M.lookup addr (memory env) of
        Just val -> val
        Nothing -> dflt

-- | Write a value to a memory address
writeMemory :: IS.Address -> a -> Environment a -> Environment a
writeMemory addr val env =
    Environment {
        registers = registers env,
        memory = M.insert addr val (memory env)
    }

-- | An interpreter "executes" instructions and updates its environment as a
-- result
class Interpreter a where
    -- | Given an environment, "execute" an instruction and return a new,
    -- updated environment. This function should describe the language's big
    -- step operational semantics
    execute :: IS.Instruction -> Environment a -> Environment a

-- | This interpreter instance essentially describes an idealised version of
-- REIL's concrete semantics. We ignore issues such as overflow, etc.
--
-- TODO how to handle the operand size?
instance Interpreter Int where
    -- Add instruction
    execute (IS.Add (IS.IntegerLiteral i1 _)
                    (IS.IntegerLiteral i2 _)
                    (IS.Register r _)) env =
        writeRegister r (i1 + i2) env
    execute (IS.Add (IS.IntegerLiteral i _)
                    (IS.Register r1 _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (i + readRegister r1 env) env
    execute (IS.Add (IS.Register r1 _)
                    (IS.IntegerLiteral i _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (readRegister r1 env + i) env
    execute (IS.Add (IS.Register r1 _)
                    (IS.Register r2 _)
                    (IS.Register r3 _)) env =
        writeRegister r3 (readRegister r1 env + readRegister r2 env) env
    -- And instruction
    execute (IS.And (IS.IntegerLiteral i1 _)
                    (IS.IntegerLiteral i2 _)
                    (IS.Register r _)) env =
        writeRegister r (i1 .&. i2) env
    execute (IS.And (IS.IntegerLiteral i _)
                    (IS.Register r1 _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (i .&. readRegister r1 env) env
    execute (IS.And (IS.Register r1 _)
                    (IS.IntegerLiteral i _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (readRegister r1 env .&. i) env
    execute (IS.And (IS.Register r1 _)
                    (IS.Register r2 _)
                    (IS.Register r3 _)) env =
        writeRegister r3 (readRegister r1 env .&. readRegister r2 env) env
    -- Bisz instruction
    execute (IS.Bisz (IS.IntegerLiteral i _)
                     _
                     (IS.Register r _)) env
        | i == 0 = writeRegister r 1 env
        | otherwise = writeRegister r 0 env
    execute (IS.Bisz (IS.Register r1 _)
                     _
                     (IS.Register r2 _)) env
        | val == 0 = writeRegister r2 1 env
        | otherwise = writeRegister r2 0 env
        where val = readRegister r1 env
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
        writeRegister r (i1 `quot` i2) env
    execute (IS.Div (IS.IntegerLiteral i _)
                    (IS.Register r1 _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (i `quot` readRegister r1 env) env
    execute (IS.Div (IS.Register r1 _)
                    (IS.IntegerLiteral i _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (readRegister r1 env `quot` i) env
    execute (IS.Div (IS.Register r1 _)
                    (IS.Register r2 _)
                    (IS.Register r3 _)) env =
        writeRegister r3 (readRegister r1 env `quot` readRegister r2 env) env
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
        writeRegister r (readMemory i 0 env) env
    execute (IS.Ldm (IS.Register r1 _)
                    _
                    (IS.Register r2 _)) env =
        writeRegister r2 (readMemory (readRegister r1 env) 0 env) env
    -- Mod instruction
    execute (IS.Mod (IS.IntegerLiteral i1 _)
                    (IS.IntegerLiteral i2 _)
                    (IS.Register r _)) env =
        writeRegister r (i1 `mod` i2) env
    execute (IS.Mod (IS.IntegerLiteral i _)
                    (IS.Register r1 _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (i `mod` readRegister r1 env) env
    execute (IS.Mod (IS.Register r1 _)
                    (IS.IntegerLiteral i _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (readRegister r1 env `mod` i) env
    execute (IS.Mod (IS.Register r1 _)
                    (IS.Register r2 _)
                    (IS.Register r3 _)) env =
        writeRegister r3 (readRegister r1 env `mod` readRegister r2 env) env
    -- Mul instruction
    execute (IS.Mul (IS.IntegerLiteral i1 _)
                    (IS.IntegerLiteral i2 _)
                    (IS.Register r _)) env =
        writeRegister r (i1 * i2) env
    execute (IS.Mul (IS.IntegerLiteral i _)
                    (IS.Register r1 _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (i * readRegister r1 env) env
    execute (IS.Mul (IS.Register r1 _)
                    (IS.IntegerLiteral i _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (readRegister r1 env * i) env
    execute (IS.Mul (IS.Register r1 _)
                    (IS.Register r2 _)
                    (IS.Register r3 _)) env =
        writeRegister r3 (readRegister r1 env * readRegister r2 env) env
    -- Nop instruction
    execute (IS.Nop _ _ _) env =
        env
    -- Or instruction
    execute (IS.Or (IS.IntegerLiteral i1 _)
                   (IS.IntegerLiteral i2 _)
                   (IS.Register r _)) env =
        writeRegister r (i1 .|. i2) env
    execute (IS.Or (IS.IntegerLiteral i _)
                   (IS.Register r1 _)
                   (IS.Register r2 _)) env =
        writeRegister r2 (i .|. readRegister r1 env) env
    execute (IS.Or (IS.Register r1 _)
                   (IS.IntegerLiteral i _)
                   (IS.Register r2 _)) env =
        writeRegister r2 (readRegister r1 env .|. i) env
    execute (IS.Or (IS.Register r1 _)
                   (IS.Register r2 _)
                   (IS.Register r3 _)) env =
        writeRegister r3 (readRegister r1 env .|. readRegister r2 env) env
    -- Stm instruction
    execute (IS.Stm (IS.IntegerLiteral i1 _)
                    _
                    (IS.IntegerLiteral i2 _)) env =
        writeMemory i2 i1 env
    execute (IS.Stm (IS.IntegerLiteral i _)
                    _
                    (IS.Register r _)) env =
        writeMemory (readRegister r env) i env
    execute (IS.Stm (IS.Register r _)
                    _
                    (IS.IntegerLiteral i _)) env =
        writeMemory i (readRegister r env) env
    execute (IS.Stm (IS.Register r1 _)
                    _
                    (IS.Register r2 _)) env =
        writeMemory (readRegister r2 env) (readRegister r1 env) env
    -- Str instruction
    execute (IS.Str (IS.IntegerLiteral i _)
                    _
                    (IS.Register r _)) env =
        writeRegister r i env
    execute (IS.Str (IS.Register r1 _)
                    _
                    (IS.Register r2 _)) env =
        writeRegister r2 (readRegister r1 env) env
    -- Sub instruction
    execute (IS.Sub (IS.IntegerLiteral i1 _)
                    (IS.IntegerLiteral i2 _)
                    (IS.Register r _)) env =
        writeRegister r (i1 - i2) env
    execute (IS.Sub (IS.IntegerLiteral i _)
                    (IS.Register r1 _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (i - readRegister r1 env) env
    execute (IS.Sub (IS.Register r1 _)
                    (IS.IntegerLiteral i _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (readRegister r1 env - i) env
    execute (IS.Sub (IS.Register r1 _)
                    (IS.Register r2 _)
                    (IS.Register r3 _)) env =
        writeRegister r3 (readRegister r1 env - readRegister r2 env) env
    -- Undef instruction
    execute (IS.Undef _ _ (IS.Register r _)) env =
        Environment {
            registers = M.delete r (registers env),
            memory = memory env
        }
    -- Unkn instruction
    execute (IS.Unkn _ _ _) env =
        env
    -- Xor instruction
    execute (IS.Xor (IS.IntegerLiteral i1 _)
                    (IS.IntegerLiteral i2 _)
                    (IS.Register r _)) env =
        writeRegister r (i1 `xor` i2) env
    execute (IS.Xor (IS.IntegerLiteral i _)
                    (IS.Register r1 _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (i `xor` readRegister r1 env) env
    execute (IS.Xor (IS.Register r1 _)
                    (IS.IntegerLiteral i _)
                    (IS.Register r2 _)) env =
        writeRegister r2 (readRegister r1 env `xor` i) env
    execute (IS.Xor (IS.Register r1 _)
                    (IS.Register r2 _)
                    (IS.Register r3 _)) env =
        writeRegister r3 (readRegister r1 env `xor` readRegister r2 env) env
    -- Error
    execute inst _ =
        error $ "Invalid instruction: " ++ show inst
