module SGLR.Engine where
{-|
Module      : SGLR.Engine
Description : The parsing engine for this SGLR parser implementation.
Copyright   : (c) Jeff Smits, 2014
Licence     : MIT
Maintainer  : jeff.smits@gmail.com
Stability   : experimental
-}

import qualified Data.WordMap.Strict as WordMap
--import qualified Data.Array.IArray as A
import Data.Array.IArray ((!))
import qualified Data.Maybe as Maybe
-- import Debug.Trace (traceShow)

import qualified SGLR.Model as M
import SGLR.Model (Instruction(..), StackElem(..))
import qualified Data.ByteString.Lazy as B
import Data.Word

-- inspired by foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
-- but using ByteString instead of [a] (therefore Word8 instead of a)
-- specialised for Either (Maybe c) as the instance of Monad m
-- and I reordered the inputs of the function argument because that suited me
bsFoldM :: (Word8 -> b -> Either (Maybe c) b) -> b -> B.ByteString -> Either (Maybe c) b
bsFoldM f def = B.foldl' (\mb a -> (f a) =<< mb) (Right def)

-- the top-level function. 
-- Takes all the tables and the input, 
-- produces a Just with the result or Nothing in case of failure
runParser :: M.InstrTable a -> M.GotoTable a -> M.EOFTable a -> B.ByteString -> Maybe a
runParser instrTable gotoTable eofTable input = case eofParse =<< bsFoldM parse stack input of
  Left v  -> v
  Right _ -> Nothing -- error "runParser: Parse failed, no Accept or Error was hit! "
  where stack = [(instrTable ! 0, gotoTable ! 0, WordMap.lookup 0 eofTable, Inp 0)]

-- get and instruction from a table, and return the 
-- Error instruction if the given key has no corresponding instruction
getInstr :: WordMap.Key -> WordMap.WordMap (Instruction a) -> Instruction a
getInstr = WordMap.findWithDefault Error

-- parse an input symbol, given that input symbol and a parse stack
-- uses the generic parse' and gives it:
-- * a continuation function for when the input is not consumed (e.g. when a reduce instruction is done)
-- * a shift execution action
-- * an instruction to execute
-- * the parse stack
parse :: M.Input -> M.Stack a -> Either (Maybe a) (M.Stack a)
parse inp stack@((i2i,_,_,_):_) =
  parse' 
    (parse inp) 
    (\i2i' gt' mI' stack' -> (i2i',gt',mI',Inp inp) : stack')
    (getInstr (fromIntegral inp) i2i) 
    stack
parse _ [] = error "parse: implementors logic is flawed or a bug was introduced later"

-- parse at EOF (the end of the input), given a parse stack
-- uses the generic parse' and gives it:
-- * a continuation function for when the input is not consumed (e.g. when a reduce instruction is done)
-- * a shift execution action, in this case a runtime error because no shift instruction should be in the eofTable
-- * an instruction to execute
-- * the parse stack
eofParse :: M.Stack a -> Either (Maybe a) (M.Stack a)
eofParse stack@((_,_,mI,_):_) =
  parse' 
    eofParse 
    (error "eofParse: Shift instruction in the EOFTable") 
    (Maybe.fromMaybe Error mI) 
    stack
eofParse [] = error "eofParse: implementors logic is flawed or a bug was introduced later"

-- does the actual shift/reduce kind of action.
-- takes:
-- * a continuation function for when the input is not consumed (e.g. when a reduce instruction is found)
-- * a shift execution function
-- * an instruction
-- * a stack
-- dispatches on the kind of instruction. 
-- Stops with a Left when Accept or Error is found
-- Gives a Right (signaling to continue because not yet done) with the new stack
parse' :: (M.Stack a -> Either (Maybe a) (M.Stack a))
       -> (M.InputToInstr a -> M.Goto a -> M.EOF a -> M.Stack a -> M.Stack a)
       ->  M.Instruction a
       ->  M.Stack a
       -> Either (Maybe a) (M.Stack a)
parse' cont shift instr stack = case instr of
  Shift  (i2i', gt', mI') -> Right $ shift i2i' gt' mI' stack
  Reduce r                -> reduce cont r stack
  Accept                  -> Left $ Just $ (\(_,_,_,Trm (_,v)) -> v) $ head stack
  Error                   -> Left Nothing

-- does a reduce action. (code is a little messy)
-- take the parse function to continue with, the rule to reduce with, and the stack
-- unpack the rule, split the stack into the amount the rule needs and the rest
-- get the actual things the rule needs from the stack elements and perform the reduce action
-- find the state to goto from the head of the rest of the stack
-- if there isn't a state to goto return Left Nothing (an Error), 
-- else push the result of the action on the stack and wrap in a Right
reduce :: (M.Stack a -> Either (Maybe a) (M.Stack a))
       -> M.Rule a
       -> M.Stack a
       -> Either (Maybe a) (M.Stack a)
reduce parseFun r stack = parseFun =<< reductionResult
  where (n, srt, action) = r
        (ruleArgs,stack') = splitAt (fromIntegral n) stack
        term = action $ map (\(_,_,_,a) -> a) $ ruleArgs
        (_,M.Goto gt,_,_) = head stack'
        maybeGotoState = WordMap.lookup srt gt
        pushResult (i2i',gt',mI') = Right $ (i2i', gt', mI', Trm term) : stack'
        reductionResult = Maybe.maybe (Left Nothing) pushResult maybeGotoState
