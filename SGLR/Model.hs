module SGLR.Model where
{-|
Module      : SGLR.Model
Description : The model (types) for this SGLR parser implementation
Copyright   : (c) Jeff Smits, 2014
Licence     : MIT
Maintainer  : jeff.smits@gmail.com
Stability   : experimental

Extensions : UnboxedTuples
-}

import Data.Word
import Data.Array
import Data.WordMap.Strict

-- unused for now
{-# LANGUAGE UnboxedTuples #-}

type Input  = Word8    -- ^ The input type, using what's in ByteStrings
type Sort   = Word     -- ^ A Sort, usually a capital letter in BNF
type State  = Word     -- ^ A state in the automaton that the SGLR parsing engine executes
type Term a = (Sort,a) -- ^ A Term, made out of a Sort and the value constructed from the reduction

-- | The LR stack, holding the state,
--   a map from input to instruction,
--   the stackelement
type Stack     a = [(State, InputToInstr a, StackElem a)]
data StackElem a = Inp Input | Trm (Term a) deriving (Show) -- ^ A sum of Sort and Input

-- | A number of stack elements to pop,
--   the sort it produces,
--   the action to perform on those stackelements
type Rule a = (Word, Goto a, [StackElem a] -> Term a)

-- | The possible instructions in the state/input table
--   The table for state/sort only contains Goto instruction and are therefore unlabeled
data Instruction a = Shift  (State, InputToInstr a)
                   | Reduce (Rule a)
                   | Accept
                   | Error
instance Show a => Show (Instruction a) where
  show (Shift i2i)      = "Shift <WordMap>"
  show (Reduce (w,g,_)) = "Reduce (" ++ show w ++ ",<WordMap>,<(->)>)"
  show Accept = "Accept"
  show Error = "Error"

-- | A mapping of State -> (sparse) Input -> Instruction
type InstrTable   a = Array Word (InputToInstr a)
type InputToInstr a = WordMap (Instruction a)
-- ^ internal sparse Input -> Instruction mapping

-- | A mapping of Sort -> (sparse) State -> State
type GotoTable a = Array Word (Goto a)
type Goto      a = WordMap (State, InputToInstr a)
-- ^ internal sparse State -> State       mapping

-- | A mapping of State -> Instruction at the EOF
type EOFTable a = WordMap (Instruction a)
