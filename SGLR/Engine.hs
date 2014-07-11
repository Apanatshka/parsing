module SGLR.Engine where

import qualified Data.WordMap.Strict as WordMap
import qualified Data.Array.IArray as A
import Data.Array.IArray ((!))
import qualified Data.Maybe as Maybe
import Debug.Trace (traceShow)

import qualified SGLR.Model as M
import SGLR.Model (Instruction(..), StackElem(..))
import qualified Data.ByteString.Lazy as B
import Data.Word

bsFoldM :: (Word8 -> b -> Either (Maybe c) b) -> b -> B.ByteString -> Either (Maybe c) b
bsFoldM f def = B.foldl' (\mb a -> (f a) =<< mb) (Right def)

-- LR Parsing Engine
runParser :: M.InstrTable a -> M.EOFTable a -> B.ByteString -> Maybe a
runParser instrTable oefTable input = case bsFoldM parse [(0, instrTable ! 0, Inp 0)] input >>= eofParse oefTable of
  Left v  -> v
  Right _ -> error "Parse failed: no Accept or Error was hit! "

getInstr = WordMap.findWithDefault Error

parse :: M.Input -> M.Stack a -> Either (Maybe a) (M.Stack a)
parse inp stack@((_,i2i,_):_) =
  parse' 
    (parse inp) 
    (\st' i2i' stack -> (st',i2i',Inp inp) : stack) 
    (getInstr (fromIntegral inp) i2i) 
    stack

eofParse :: M.EOFTable a -> M.Stack a -> Either (Maybe a) (M.Stack a)
eofParse table stack@((state,_,_):_) =
  parse' 
    (eofParse table) 
    (error "Shift instruction in the EOFTable") 
    (getInstr state table) 
    stack

parse' :: (M.Stack a -> Either (Maybe a) (M.Stack a))
       -> (M.State -> M.InputToInstr a -> M.Stack a -> M.Stack a)
       ->  M.Instruction a
       ->  M.Stack a -> Either (Maybe a) (M.Stack a)
parse' cont shift instr stack = case instr of
  Shift  (st',i2i') -> Right $ shift st' i2i' stack
  Reduce r          -> reduce cont r stack
  Accept            -> Left $ Just $ (\(_,_,Srt (_,v)) -> v) $ head stack
  Error             -> Left Nothing

reduce :: (M.Stack a -> Either (Maybe a) (M.Stack a)) -> M.Rule a -> M.Stack a -> Either (Maybe a) (M.Stack a)
reduce parseFun r stack = parseFun =<< reductionResult
  where (n, goto, action) = r
        (ruleArgs,stack') = splitAt (fromIntegral n) stack
        sort = action $ map (\(_,_,a) -> a) $ ruleArgs
        state = (\(a,_,_) -> a) $ head stack'
        maybeGotoState = WordMap.lookup state goto
        doReduction (gotoState,i2i) = Right $ (gotoState, i2i, Srt sort) : stack'
        reductionResult = Maybe.maybe (Left Nothing) doReduction maybeGotoState
