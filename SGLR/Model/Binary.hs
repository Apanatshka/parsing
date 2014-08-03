{-# LANGUAGE DeriveGeneric #-}
module SGLR.Model.Binary where
{-|
Module      : SGLR.Model
Description : The model (types) for this SGLR parser implementation
Copyright   : (c) Jeff Smits, 2014
Licence     : MIT
Maintainer  : jeff.smits@gmail.com
Stability   : experimental

Extensions  : DeriveGeneric
-}

import Data.Binary (Binary, put, get)
import GHC.Generics (Generic)
import Control.Monad

import Data.Word (Word, Word8)

import Data.WordMap.Strict (WordMap)
import qualified Data.WordMap.Strict as WordMap

import Data.List (genericLength)
import Data.Array (Ix)
import Data.Array (Array)
import Data.Array.BitArray (BitArray)
import qualified Data.Array.BitArray as BitArray
import qualified Data.Array.BitArray.ByteString as BitArray
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSChar

import qualified SGLR.TableGen.Rule as TGRule

-- adapted from binary-0.7.2.1, Data.Binary.Class, the IntMap instance
instance (Binary e) => Binary (WordMap e) where
  put m = put (WordMap.size m) >> mapM_ put (WordMap.toAscList m)
  get   = liftM WordMap.fromDistinctAscList get

-- inpired by binary-0.7.2.1, Data.Binary.Class, the UArray instance
instance (Binary i, Ix i) => Binary (BitArray i) where
  put m = put (BitArray.bounds m) >> put (BitArray.toByteString m)
  get   = liftM2 BitArray.fromByteString get get

-- basic instances of Eq, Ord, Show using transformation to bytestring
instance (Ix i) => Eq (BitArray i) where
  a == b = (BitArray.toByteString a) == (BitArray.toByteString b)

instance (Ix i) => Ord (BitArray i) where
  a <= b = (BitArray.toByteString a) <= (BitArray.toByteString b)

instance (Ix i) => Show (BitArray i) where
  show a = show (BitArray.toByteString a)

-- | The full "parse table", all info the engine (might/)will need.
type ParseTable = (InstrTable, EOFTable, GotoTable, Rules)

type Input  = Word8 -- ^ The input type, using what's in ByteStrings
type Sort   = Word  -- ^ A Sort, usually a capital letter in BNF
type State  = Word  -- ^ A state in the automaton that the SGLR parsing engine executes

-- | A reduction rule, consisting of the amount of data popped from the
-- stack, the sort being constructed and the actual action to be
-- performed on the data.
type Rule = (Word, Sort, Action)

data Action = Cons  !ByteString                  -- ^ Build a constructor around the data using the given name
            | Cons' !ByteString !(BitArray Word) -- ^ Build a constructor around part of the data
            | Pick  !Word                        -- ^ Pick one part of the data (0-indexed)
            | Drop                               -- ^ Drop the data
            deriving (Generic, Eq, Ord, Show)
instance Binary Action -- Generic derivation

type Rules = Array Word Rule

-- | The possible instructions in the state/input table
--   The table for state/sort only contains Goto instruction and are therefore unlabeled
data Instr = Shift  !State -- ^ shift and move to the given state
           | Reduce !Word  -- ^ reduce by the given rule
           | Accept        -- ^ accept
           -- | Error      -- ^ error. Not used.
           deriving (Generic, Eq, Ord, Show)
instance Binary Instr -- Generic derivation

-- | A mapping of State -> (sparse) Input -> Instruction
type InstrTable = Array Word (WordMap Instr)

-- | A mapping of State -> (sparse) Sort -> State
type GotoTable = Array Word (WordMap State)

-- | A mapping of State -> Instruction at the EOF
type EOFTable = WordMap Instr

-- | 
toRule :: (Enum sort) => TGRule.Rule' sort lit -> Rule
toRule r = case TGRule.fromRule' r of
  TGRule.Cons s l c -> (genericLength l, fromIntegral $ fromEnum s, Cons $ BSChar.pack c)
  TGRule.IsA  s1 _  -> (1, fromIntegral $ fromEnum s1, Pick 0)
  TGRule.StartRule _-> error "toRule: implementors logic is flawed or a bug was introduced later"
