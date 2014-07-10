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

-- LR Parsing Engine

runParser :: Show a => M.InstrTable a -> B.ByteString -> Maybe a
runParser instrTable input = (\(Left b) -> b) $ bsFoldM parse [(0, instrTable ! 0, Inp 0)] input

bsFoldM :: (Word8 -> b -> Either (Maybe c) b) -> b -> B.ByteString -> Either (Maybe c) b
bsFoldM f def = B.foldl' (\mb a -> (f a) =<< mb) (Right def)

getInstr = WordMap.findWithDefault Error

parse :: Show a => M.Input -> M.Stack a -> Either (Maybe a) (M.Stack a)
parse inp stack@((_,i2i,_):_) = case getInstr (fromIntegral inp) i2i of
  Shift  (st',i2i') -> Right $ (st', i2i', Inp inp) : stack
  Reduce r          -> reduce inp stack r
  Accept            -> Left $ Just $ (\(_,_,Srt (_,v)) -> v) $ head stack
  Error             -> Left Nothing

reduce :: Show a => M.Input -> M.Stack a -> M.Rule a -> Either (Maybe a) (M.Stack a)
reduce inp stack r = parse inp =<< reductionResult
  where (n, goto, action) = r
        (ruleArgs,stack') = splitAt (fromIntegral n) stack
        sort = action $ map (\(_,_,a) -> a) $ ruleArgs
        state = (\(a,_,_) -> a) $ head stack'
        maybeGotoState = WordMap.lookup state goto
        doReduction (gotoState,i2i) = Right $ (gotoState, i2i, Srt sort) : stack'
        reductionResult = Maybe.maybe (Left Nothing) doReduction maybeGotoState
