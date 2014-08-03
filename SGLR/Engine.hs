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

bsFoldM :: (Word8 -> b -> Either (Maybe c) b) -> b -> B.ByteString -> Either (Maybe c) b
bsFoldM f def = B.foldl' (\mb a -> (f a) =<< mb) (Right def)

-- LR Parsing Engine
runParser :: M.InstrTable a -> M.GotoTable a -> M.EOFTable a -> B.ByteString -> Maybe a
runParser instrTable gotoTable eofTable input = case eofParse =<< bsFoldM parse stack input of
  Left v  -> v
  Right _ -> error "runParser: Parse failed, no Accept or Error was hit! "
  where stack = [(instrTable ! 0, gotoTable ! 0, WordMap.lookup 0 eofTable, Inp 0)]

getInstr :: WordMap.Key -> WordMap.WordMap (Instruction a) -> Instruction a
getInstr = WordMap.findWithDefault Error

parse :: M.Input -> M.Stack a -> Either (Maybe a) (M.Stack a)
parse inp stack@((i2i,_,_,_):_) =
  parse' 
    (parse inp) 
    (\i2i' gt' mI' stack' -> (i2i',gt',mI',Inp inp) : stack')
    (getInstr (fromIntegral inp) i2i) 
    stack
parse _ [] = error "parse: implementors logic is flawed or a bug was introduced later"

eofParse :: M.Stack a -> Either (Maybe a) (M.Stack a)
eofParse stack@((_,_,mI,_):_) =
  parse' 
    eofParse 
    (error "eofParse: Shift instruction in the EOFTable") 
    (Maybe.fromMaybe Error mI) 
    stack
eofParse [] = error "eofParse: implementors logic is flawed or a bug was introduced later"

parse' :: (M.Stack a -> Either (Maybe a) (M.Stack a))
       -> (M.InputToInstr a -> M.Goto a -> M.EOF a -> M.Stack a -> M.Stack a)
       ->  M.Instruction a
       ->  M.Stack a -> Either (Maybe a) (M.Stack a)
parse' cont shift instr stack = case instr of
  Shift  i2i' gt' mI' -> Right $ shift i2i' gt' mI' stack
  Reduce r            -> reduce cont r stack
  Accept              -> Left $ Just $ (\(_,_,_,Trm (_,v)) -> v) $ head stack
  Error               -> Left Nothing

reduce :: (M.Stack a -> Either (Maybe a) (M.Stack a)) -> M.Rule a -> M.Stack a -> Either (Maybe a) (M.Stack a)
reduce parseFun r stack = parseFun =<< reductionResult
  where (n, srt, action) = r
        (ruleArgs,stack') = splitAt (fromIntegral n) stack
        term = action $ map (\(_,_,_,a) -> a) $ ruleArgs
        (_,M.Goto gt,_,_) = head stack'
        maybeGotoState = WordMap.lookup srt gt
        doReduction (i2i',gt',mI') = Right $ (i2i', gt', mI', Trm term) : stack'
        reductionResult = Maybe.maybe (Left Nothing) doReduction maybeGotoState
