module SGLR.TableGen where
{-|
Module      : SGLR.TableGen
Description : The parse table generator for this SGLR parser implementation. 
Copyright   : (c) Jeff Smits, 2014
Licence     : MIT
Maintainer  : jeff.smits@gmail.com
Stability   : experimental
-}

import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Either as Either
import Data.Array.IArray (IArray, Ix)
import qualified Data.Array.IArray as IArray
import Data.WordMap (WordMap)
import qualified Data.WordMap as WordMap
import Data.Word (Word)

import qualified SGLR.TableGen.Graph as Graph
import SGLR.TableGen.Automaton (NFA, NFATrans(..), AutoState(..))
import qualified SGLR.TableGen.Automaton as Auto
import SGLR.TableGen.Rule (Rule(..), RPart(..), Rule')
import qualified SGLR.TableGen.Rule as Rule
import SGLR.Model.Binary (ParseTable, InstrTable, EOFTable, GotoTable)
import qualified SGLR.Model.Binary as BModel

-- import Debug.Trace (trace, traceShow)

-- debug a = traceShow a a

pairWithInput :: (a -> b) -> a -> (a,b)
pairWithInput f i = (i, f i)

sortMapping1 :: (Ord k, Ord a) => [k] -> [a] -> Map k (Set a)
sortMapping1 ruleSorts = Map.fromListWith (Set.union) . zip ruleSorts . map Set.singleton

sortMapping2 :: Ord k => [k] -> t -> (t -> k -> a) -> Map k a
sortMapping2 ruleSorts sortMap f = Map.fromList $ map (pairWithInput (f sortMap)) ruleSorts

get :: Ord k => k -> Map k c -> c
get k = Maybe.fromJust . Map.lookup k

wmget :: Word -> WordMap e -> e
wmget k = Maybe.fromJust . WordMap.lookup k

splits :: [a] -> [(Int,[a])]
splits l = zip (map length $ List.inits l) (List.tails l)

arrayFromList :: (IArray a e) => [e] -> a Word e
arrayFromList l = IArray.listArray (0, List.genericLength l - 1) l

emptyArray :: (IArray a e, Ix i) => (i,i) -> e -> a i e
emptyArray bounds item = IArray.listArray bounds $ repeat item

rulesToTables :: (Enum sort, Bounded sort, Ord sort, Enum lit, Ord lit, Show lit)
              => [Rule' sort lit]
              -> sort
              -> ParseTable
rulesToTables rule's startSymbol = (iT, eT, gT, mbRules)
  where rules      = map Rule.fromRule' rule's
        ruleSorts  = map Rule.ruleSort rules
        sortMap    = sortMapping1 ruleSorts rules
        sortToRNo  = sortMapping1 ruleSorts [0..]
        firsts     = sortMapping2 ruleSorts sortMap firstLits2
        follows    = followSet firsts rules startSymbol
        dfa        = Auto.toDfa (nfa rules startSymbol sortToRNo)
        mbRules    = arrayFromList $ map BModel.toRule rule's
        dfa'       = map withOutE $ Graph.nodes dfa
        withOutE   = pairWithInput (Graph.outE dfa . fst)
        (iT,eT,gT) = List.foldl' (flip $ nodesToTables follows rules) emptyTs dfa'
        emptyTs    = ( emptyArray (0, List.genericLength dfa' - 1) WordMap.empty
                     , WordMap.empty
                     , emptyArray (0, List.genericLength dfa' - 1) WordMap.empty )
        -- this is a hack around not being able to just say (maxBound :: sort)
        maxSort :: (Enum s, Bounded s) => s -> s
        maxSort _ = maxBound
        maxSrt = fromIntegral $ fromEnum $ maxSort $ head ruleSorts

nodesToTables :: (Ord lit, Enum lit, Ord sort, Enum sort, Show lit)
              => Map sort (Set (RPart sort lit))
              -> [Rule sort lit]
              -> (Graph.Node (Auto.AutoState (Set (Integer, Rule sort lit))), Set (Graph.Adj (RPart sort lit)))
              -> (InstrTable, EOFTable, GotoTable)
              -> (InstrTable, EOFTable, GotoTable)
nodesToTables follows rules ((state, setOfRules), setOfEdges) (instrArr, eofWM, gotoArr) =
  (instrArr'', eofWM'', gotoArr')
  where eitherRules = map ruleToEither $ Set.toList $ Auto.getLabel setOfRules
        ruleToEither (i,r) = case i `compare` List.genericLength (Rule.ruleBody r) of
          EQ -> Right (fromIntegral $ Maybe.fromJust $ List.elemIndex r rules
                      , Set.toList $ get (Rule.ruleSort r) follows)
          LT -> Left  (fromIntegral $ Maybe.fromJust $ List.elemIndex r rules
                      , Set.toList $ get (Rule.ruleSort r) follows)
          GT -> error $ "nodesToTables: Something went wrong in the " ++
                      "generation of the Automaton. The dot was put " ++
                      "further than the body of the rule has items. "
        (instrArr',  eofWM', gotoArr') = Set.foldr (edgeToInstr state') 
                        (instrArr, eofWM, gotoArr) setOfEdges
        (instrArr'', eofWM'') = List.foldr (ruleToReduce state') 
                        (instrArr', eofWM') (Either.rights eitherRules)
        state' = fromIntegral state

edgeToInstr :: (Enum lit, Enum srt, Integral st2)
            => Word
            -> (st2, RPart srt lit)
            -> (InstrTable, EOFTable, GotoTable)
            -> (InstrTable, EOFTable, GotoTable)
edgeToInstr state (to, rpart) (instrArr, eofWM, gotoArr) = 
  case rpart of
    Srt s -> (instrArr, eofWM, gotoArr IArray.// [(state, WordMap.insert gotee toState gotoWM)])
      where gotee  = fromIntegral $ fromEnum s
            gotoWM = gotoArr IArray.! state
    Lit l -> (instrArr IArray.// [(state, WordMap.insert shiftee (BModel.Shift toState) instrWM)], eofWM, gotoArr)
      where shiftee = fromIntegral $ fromEnum l
            instrWM = instrArr IArray.! state
    EOF   -> (instrArr, WordMap.insert state BModel.Accept eofWM, gotoArr)
  where toState = fromIntegral to

ruleToReduce :: (Enum lit, Enum srt, Show lit)
             => Word
             -> (Word,[RPart srt lit])
             -> (InstrTable, EOFTable)
             -> (InstrTable, EOFTable)
ruleToReduce state (ruleNo, ruleFollows) (instrArr, eofWM) = 
  List.foldr (followToReduce state ruleNo) (instrArr, eofWM) ruleFollows

followToReduce :: (Enum lit, Enum srt, Show lit)
               => Word
               -> Word
               -> RPart srt lit
               -> (InstrTable, EOFTable)
               -> (InstrTable, EOFTable)
followToReduce state ruleNo follow (instrArr, eofWM) = case follow of
  Lit lit -> (instrArr IArray.// [(state, WordMap.alter alterFun followLit instrWM)], eofWM)
    where followLit   = fromIntegral $ fromEnum lit
          instrWM     = instrArr IArray.! state
          alterFun mI = case mI of
            Nothing                -> Just $ BModel.Reduce ruleNo
            Just (BModel.Shift _)  -> error $ "ruleToReduce: "
                                           ++ "Shift-Reduce problem "
                                           ++ "found in state "
                                           ++ show state
                                           ++ " for input " ++ show lit
            Just (BModel.Reduce _) -> error $ "ruleToReduce: "
                                           ++ "Reduce-Reduce problem "
                                           ++ "found in state "
                                           ++ show state
                                           ++ " for input " ++ show lit
            Just BModel.Accept -> error $ "ruleToReduce: implementors "
                                       ++ "logic is flawed or a bug "
                                       ++ "was introduced later"
  EOF     -> (instrArr, WordMap.alter alterFun state eofWM)
    where alterFun mI = case mI of
            Nothing                -> Just $ BModel.Reduce ruleNo
            Just (BModel.Reduce _) -> error $ "ruleToReduce: "
                                           ++ "Reduce-Reduce problem "
                                           ++ "found in state "
                                           ++ show state ++ " for EOF"
            Just BModel.Accept     -> error $ "ruleToReduce: "
                                           ++ "Accept-Reduce problem "
                                           ++ "found in state "
                                           ++ show state ++ " for EOF"
            Just (BModel.Shift _) -> error $ "ruleToReduce: "
                                          ++ "implementors logic is "
                                          ++ "flawed or a bug was "
                                          ++ "introduced later"
  Srt _ -> error $ "ruleToReduce: FollowSet was not correctly"
                ++ " calculated: it contains Sorts where only Literals "
                ++ "and EOF are expected. "

nfa :: (Enum sort, Bounded sort, Ord sort, Enum lit, Ord lit)
    => [Rule sort lit]
    -> sort
    -> Map sort (Set Integer)
    -> NFA (Integer, Rule sort lit) (RPart sort lit)
nfa rules startSymbol sortToRuleNo = Graph.fromLGraph $ addInitial $ addAccept basicGraph
  where basicGraph = List.foldl' (flip addRule) (List.genericLength rules, ([],[])) indexedRules
        indexedRules = (zip [0..] rules)
        addRule (rn,r) (n,g) = (n + Rule.ruleSize r, addRuleEdges r n rn (addRuleNodes r n rn g))
        addRuleNodes r n rn = Graph.lAddNodes ((rn,h):(zip [n..] t))
          where (h:t) = map (Normal . flip (,) r) [0 .. Rule.ruleSize r]
        addRuleEdges r n sn = Graph.lAddEdges $ ruleEdge =<< (zip (sn:[n..]) $ zip [n..] $ Rule.ruleBody r)
        ruleEdge (n1, (n2, part)) = (n1, n2, NFATrans part) : case part of
          Lit _ -> []
          Srt s -> map (\n2' -> (n1,n2',Eps)) $ Set.toList $ get s sortToRuleNo
          EOF -> error "nfa: implementors logic is flawed or a bug was introduced later"
        initials = map fromIntegral $ List.elemIndices startSymbol $ map Rule.ruleSort rules
        fakeRule = StartRule startSymbol
        addAccept (n,g) = (n+1, Graph.lAddEdges (map (\from -> (from,n+1,NFATrans $ Srt startSymbol)) initials) $ Graph.lAddNodes [(n+1, Final (1,fakeRule))] g)
        addInitial (n,g) = Graph.lAddEdges (map (\to -> (n+1,to,Eps)) initials) $ Graph.lAddNodes [(n+1, Initial (0,fakeRule))] g

firstLits1 :: (Ord lit, Ord sort)
            => Map sort (Set (Rule sort lit))
            -> sort 
            -> Set lit
firstLits1 sortMap sort = Set.map Rule.fromLit lits
  where lits = Set.filter Rule.isLit $ firstHelper sortMap sort

firstSorts :: (Ord lit, Ord sort)
           => Map sort (Set (Rule sort lit))
           -> sort 
           -> Set sort
firstSorts sortMap sort = Set.filter (/= sort) $ Set.map Rule.fromSrt sorts
  where sorts = Set.filter Rule.isSrt $ firstHelper sortMap sort

firstHelper :: (Ord sort, Ord lit, Ord k) => Map k (Set (Rule sort lit)) -> k -> Set (RPart sort lit)
firstHelper sortMap sort = Set.map (head . Rule.ruleBody) $ get sort sortMap

firstLits2 :: (Ord lit, Ord sort)
           => Map sort (Set (Rule sort lit))
           -> sort 
           -> Set lit
firstLits2 sortMap sort = Set.union lits recLits
  where lits    = firstLits1 sortMap sort
        recLits = firstLits2 sortMap =<< firstSorts sortMap sort

followSet :: (Ord lit, Ord sort)
          => Map sort (Set lit)
          -> [Rule sort lit]
          -> sort
          -> Map sort (Set (RPart sort lit))
followSet firsts rules start = result
  where result = Map.fromList ( (start, Set.singleton EOF) : rest)
        rest = map f (filter (/= start) $ List.nub $ map Rule.ruleSort rules)
        f s = (s, Set.unions $ map (getFollowers s firsts result) $ rulesWithSInBody)
          where rulesWithSInBody = filter (\r -> elem (Srt s) $ Rule.ruleBody r) rules

getFollowers :: (Ord lit, Ord sort)
             => sort
             -> Map sort (Set lit)
             -> Map sort (Set (RPart sort lit))
             -> Rule sort lit
             -> Set (RPart sort lit)
getFollowers s firsts result r = 
  followersToSet firsts result
    $ map (headRightOrLeft rs)                -- get the heads of the follow lists
    $ filter (not . elem (Srt s))             -- drop those that are s
    $ appendEmpty s rs                        -- append empty list if last list consists of s's
    $ dropWhile (not . elem (Srt s))          -- drop the list before the first s
    $ List.groupBy (bothOrNeither (Srt s)) rb -- split into lists of s and not s
  where rs = Rule.ruleSort r
        rb = Rule.ruleBody r

bothOrNeither :: Eq a => a -> a -> a -> Bool
bothOrNeither s a b = xnor (a == s) (b == s)

xnor :: Bool -> Bool -> Bool
xnor a b = not (a || b) || (a && b)

headRightOrLeft :: a -> [b] -> Either a b
headRightOrLeft k   = maybe (Left k) Right . Maybe.listToMaybe

appendEmpty :: (Eq sort, Eq lit) => sort -> sort -> [[RPart sort lit]] -> [[RPart sort lit]]
appendEmpty s k l   = 
  -- don't add empty list if recursive rule (i.e. s == k)
  if s /= k && elem (Srt s) (last l)
    then l ++ [[]]
    else l

followersToSet :: (Ord lit, Ord sort)
               => Map sort (Set lit)
               -> Map sort (Set (RPart sort lit))
               -> [Either sort (RPart sort lit)]
               -> Set (RPart sort lit)
followersToSet firsts follows followers = Set.unions
  $ flip map followers $ \e ->
    case e of
      Left k        -> get k follows
      Right (Srt s) -> Set.map Lit $ get s firsts
      Right (Lit l) -> Set.singleton (Lit l)
      Right EOF -> error $ "followersToSet: implementors logic is "
                        ++ "flawed or a bug was introduced later"
