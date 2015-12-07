{- |
Module      : $Header$
Description : REIL instruction set
Maintainer  : Adrian Herrera
Stability   : experimental
-}

-- | This module defines the REIL instruction set. The documentation is
-- primarily sourced from
-- http://www.zynamics.com/binnavi/manual/html/reil_language.htm

module Data.REIL.InstructionSet (
    Address,
    OperandSize(..),
    Operand(..),
    Instruction(..),
    getInstOp1,
    getInstOp2,
    getInstOp3,
    Stmt(..),
) where

import Numeric (showHex)

-- | For our purposes an address is simply an integer
type Address = Int

-- | Pretty-print an address
showAddress :: Address -> String
showAddress addr =
    "0x" ++ showHex addr ""

-- | The size of a REIL operand is either b1 (1 byte), b2 (2 bytes), b4 (4
-- bytes), b8 (8 bytes) or b16 (16 bytes).
--
-- Note that these terms don't appear to be used by BinNavi, which instead uses
-- byte, word, dword, qword and oword respectively
data OperandSize =
    Byte
    | Word
    | DWord
    | QWord
    | OWord

instance Show OperandSize where
    show Byte = "BYTE"
    show Word = "WORD"
    show DWord = "DWORD"
    show QWord = "QWORD"
    show OWord = "OWORD"

-- | REIL operands are defined by their size and their type. Valid types for
-- REIL operands are 'integer literal', 'register', and 'REIL offset'. An
-- operand can also be empty.
data Operand =
    Empty
    | IntegerLiteral Int OperandSize
    | Register String OperandSize
    | Offset Address

instance Show Operand where
    show Empty = "EMPTY"
    show (IntegerLiteral lit size) =
        show size ++ " " ++ show lit
    show (Register reg size) =
        show size ++ " " ++ reg
    show (Offset off) =
        "ADDRESS " ++ showAddress off

-- | The REIL instruction set knows only 17 different instructions. Each
-- instruction calculates at most one result (multiple effects like setting
-- flags are not allowed) and has exactly three operands (although some
-- operands can be empty).
data Instruction =
    -- | Addition
    Add Operand Operand Operand
    -- | Binary and
    | And Operand Operand Operand
    -- | Boolean is-zero
    | Bisz Operand Operand Operand
    -- | Binary shift
    | Bsh Operand Operand Operand
    -- | Unsigned division
    | Div Operand Operand Operand
    -- | Jump conditional
    | Jcc Operand Operand Operand
    -- | Load from memory
    | Ldm Operand Operand Operand
    -- | Modulo
    | Mod Operand Operand Operand
    -- | Unsigned multiplication
    | Mul Operand Operand Operand
    -- | No operation
    | Nop Operand Operand Operand
    -- | Bitwise or
    | Or Operand Operand Operand
    -- | Store to memory
    | Stm Operand Operand Operand
    -- | Store to register
    | Str Operand Operand Operand
    -- | Subtract
    | Sub Operand Operand Operand
    -- | Undefines a register
    | Undef Operand Operand Operand
    -- | Unknown instruction
    | Unkn Operand Operand Operand
    -- | Bitwise xor
    | Xor Operand Operand Operand

instance Show Instruction where
    show inst =
        showInst inst ++ " " ++ showInstOp1 inst ++ ", " ++
                                showInstOp2 inst ++ ", " ++
                                showInstOp3 inst

-- | Get an instruction's first operand
getInstOp1 :: Instruction -> Operand
getInstOp1 (_ op _ _) = op
getInstOp1 (And op _ _) = op
getInstOp1 (Bisz op _ _) = op
getInstOp1 (Bsh op _ _) = op
getInstOp1 (Div op _ _) = op
getInstOp1 (Jcc op _ _) = op
getInstOp1 (Ldm op _ _) = op
getInstOp1 (Mod op _ _) = op
getInstOp1 (Mul op _ _) = op
getInstOp1 (Nop op _ _) = op
getInstOp1 (Or op _ _) = op
getInstOp1 (Stm op _ _) = op
getInstOp1 (Str op _ _) = op
getInstOp1 (Sub op _ _) = op
getInstOp1 (Undef op _ _) = op
getInstOp1 (Unkn op _ _) = op
getInstOp1 (Xor op _ _) = op

-- | Get an instruction's second operand
getInstOp2 :: Instruction -> Operand
getInstOp2 (Add _ op _) = op
getInstOp2 (And _ op _) = op
getInstOp2 (Bisz _ op _) = op
getInstOp2 (Bsh _ op _) = op
getInstOp2 (Div _ op _) = op
getInstOp2 (Jcc _ op _) = op
getInstOp2 (Ldm _ op _) = op
getInstOp2 (Mod _ op _) = op
getInstOp2 (Mul _ op _) = op
getInstOp2 (Nop _ op _) = op
getInstOp2 (Or _ op _) = op
getInstOp2 (Stm _ op _) = op
getInstOp2 (Str _ op _) = op
getInstOp2 (Sub _ op _) = op
getInstOp2 (Undef _ op _) = op
getInstOp2 (Unkn _ op _) = op
getInstOp2 (Xor _ op _) = op

-- | Get an instruction's third operand
getInstOp3 :: Instruction -> Operand
getInstOp3 (Add _ _ op) = op
getInstOp3 (And _ _ op) = op
getInstOp3 (Bisz _ _ op) = op
getInstOp3 (Bsh _ _ op) = op
getInstOp3 (Div _ _ op) = op
getInstOp3 (Jcc _ _ op) = op
getInstOp3 (Ldm _ _ op) = op
getInstOp3 (Mod _ _ op) = op
getInstOp3 (Mul _ _ op) = op
getInstOp3 (Nop _ _ op) = op
getInstOp3 (Or _ _ op) = op
getInstOp3 (Stm _ _ op) = op
getInstOp3 (Str _ _ op) = op
getInstOp3 (Sub _ _ op) = op
getInstOp3 (Undef _ _ op) = op
getInstOp3 (Unkn _ _ op) = op
getInstOp3 (Xor _ _ op) = op

-- Helper functions for showing an instruction
showInst :: Instruction -> String
showInst (Add _ _ _) = "add"
showInst (And _ _ _) = "and"
showInst (Bisz _ _ _) = "bisz"
showInst (Bsh _ _ _) = "bsh"
showInst (Div _ _ _) = "div"
showInst (Jcc _ _ _) = "jcc"
showInst (Ldm _ _ _) = "ldm"
showInst (Mod _ _ _) = "mod"
showInst (Mul _ _ _) = "mul"
showInst (Nop _ _ _) = "nop"
showInst (Or _ _ _) = "or"
showInst (Stm _ _ _) = "stm"
showInst (Str _ _ _) = "str"
showInst (Sub _ _ _) = "sub"
showInst (Undef _ _ _) = "undef"
showInst (Unkn _ _ _) = "unkn"
showInst (Xor _ _ _) = "xor"

-- | Show an instruction's first operand
showInstOp1 :: Instruction -> String
showInstOp1 =
    show . getInstOp1

-- | Show an instruction's second operand
showInstOp2 :: Instruction -> String
showInstOp2 =
    show . getInstOp2

-- | Show an instruction's third operand
showInstOp3 :: Instruction -> String
showInstOp3 =
    show . getInstOp3

-- | A statement consists of an instruction at a specific address
data Stmt =
    Stmt Address Instruction

instance Show Stmt where
    show (Stmt addr inst) =
        showAddress addr ++ ": " ++ show inst
