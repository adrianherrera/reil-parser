{- |
Module      : $Header$
Description : Parses a REIL text file
Maintainer  : Adrian Herrera
Stability   : experimental
-}

-- | This module parses a text file containing REIL instructions.

module Data.REIL.Parse (
    -- TODO
) where

import Text.Parsec
import Text.Parsec.String

import Control.Applicative hiding ((<|>), many)

import Numeric (readHex)

import qualified Data.REIL.InstructionSet as IS
import qualified Data.REIL.BasicBlock as BB
import qualified Data.REIL.CFG as CFG

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

-- Applicative cons (is this name accurate?)
(<:>) :: Applicative f => f a -> f [a] -> f [a]
h <:> t =
    (:) <$> h <*> t

-- Parse 1 or more digits
digits :: Parser String
digits =
    many1 digit

-------------------------------------------------------------------------------

-- | Parse an address. An address is represented as a hexadecimal integer. E.g.
-- @0000100015C20700@
address :: Parser IS.Address
address =
    do
        addr <- many1 hexDigit
        let ((hexInt, _) : _) = readHex addr
        return hexInt

-- Type synonym for convienience
type InstructionConstructor =
    IS.Operand -> IS.Operand -> IS.Operand -> IS.Instruction

-- | Valid REIL instructions
instructionConstructor :: Parser InstructionConstructor
instructionConstructor =
    (IS.Add <$ try (string "add"))
    <|> (IS.And <$ try (string "and"))
    <|> (IS.Bisz <$ try (string "bisz"))
    <|> (IS.Bsh <$ try (string "bsh"))
    <|> (IS.Div <$ try (string "div"))
    <|> (IS.Jcc <$ try (string "jcc"))
    <|> (IS.Ldm <$ try (string "ldm"))
    <|> (IS.Mod <$ try (string "mod"))
    <|> (IS.Mul <$ try (string "mul"))
    <|> (IS.Nop <$ try (string "nop"))
    <|> (IS.Or <$ try (string "or"))
    <|> (IS.Stm <$ try (string "stm"))
    <|> (IS.Str <$ try (string "str"))
    <|> (IS.Sub <$ try (string "sub"))
    <|> (IS.Undef <$ try (string "undef"))
    <|> (IS.Unkn <$ try (string "unkn"))
    <|> (IS.Xor <$ try (string "xor"))
    <?> "a valid instruction"

-- | Parse a REIL instruction. A REIL instruction has the format
-- @inst [SIZE op1, SIZE op2, SIZE op3]@, where @inst@ is one of the valid REIL
-- instructions, @SIZE@ is the operand size and @op@ is an operand
instruction :: Parser IS.Instruction
instruction =
    do
        inst <- instructionConstructor
        _ <- spaces >> char '['
        op1 <- operand
        _ <- operandSeparator
        op2 <- operand
        _ <- operandSeparator
        op3 <- operand
        _ <- char ']'
        return $ inst op1 op2 op3
    where operandSeparator = char ',' >> spaces

-- | Parse a single REIL operand
operand :: Parser IS.Operand
operand =
    try emptyOperand
    <|> try integerLiteralOperand
    <|> try registerOperand
    <|> try offsetOperand
    <?> "a valid operand type"

-- | Parse a REIL operand size. An operand size is one of the strings "BYTE",
-- "WORD", "DWORD", "QWORD" or "OWORD"
operandSize :: Parser IS.OperandSize
operandSize =
    (IS.Byte <$ string "BYTE")
    <|> (IS.Word <$ string "WORD")
    <|> (IS.DWord <$ string "DWORD")
    <|> (IS.QWord <$ string "QWORD")
    <|> (IS.OWord <$ string "OWORD")
    <?> "a valid operand size"

-- | Parse the empty operand. The empty operand is simply the string "EMPTY"
emptyOperand :: Parser IS.Operand
emptyOperand =
    IS.Empty <$ (string "EMPTY" >> spaces)

-- | Parse an integer literal operand. An integer literal operand is a base-10
-- integer
integerLiteralOperand :: Parser IS.Operand
integerLiteralOperand =
    flip IS.IntegerLiteral <$> operandSize <* spaces <*> intLit
    where intLit = read <$> (sign <:> digits)
          sign = option '0' (char '-')

-- | Parse a register operand. A register must start by a letter, followed by
-- one or more alphanumeric characters
registerOperand :: Parser IS.Operand
registerOperand =
    flip IS.Register <$> operandSize <* spaces <*> reg
    where reg = letter <:> many1 alphaNum

-- | Parse a REIL offset operand. An offset has the format "ADDRESS xxx.yyy",
-- where "ADDRESS" denotes the fact that this operand is an offset and
-- @xxx.yyy@ is the address in decimal notation. The absolute address can be
-- calculated using the formula @xxx * 0x100 + yyy@
offsetOperand :: Parser IS.Operand
offsetOperand =
    IS.Offset <$> (string "ADDRESS" >> spaces *> decimalAddress)
    where decimalAddress = do
                        addr <- digits
                        _ <- char '.'
                        off <- digits
                        return $ read addr * 0x100 + read off

-- | Parse the separator between an address and an instruction. The separator
-- between an address and an instruction has the format @: @. The result is
-- ignored
addressInstructionSep :: Parser ()
addressInstructionSep =
    char ':' >> spaces >> return ()

-- | Parse a statement (combination of an address and an instruction - with a
-- separator in between)
statement :: Parser BB.Stmt
statement =
    BB.Stmt <$> address <* addressInstructionSep <*> instruction

-- | Parse a number of statements separated by a new line
statements :: Parser [BB.Stmt]
statements =
    sepEndBy statement newline
