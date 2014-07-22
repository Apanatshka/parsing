module SGLR.TableGen {-(
  BNFPart
, BNFRule(..)
, rulesToTables
)-} where
{-|
Module      : SGLR.TableGen
Description : The the parse table generator for this SGLR parser implementation. 
Copyright   : (c) Jeff Smits, 2014
Licence     : MIT
Maintainer  : jeff.smits@gmail.com
Stability   : experimental
-}

import qualified Data.Set.Monad as Set
import Data.Set.Monad (Set)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Array.IArray as A
import Data.Word (Word)
import qualified Data.Tuple as Tuple

import SGLR.TableGen.Graph (Graph, LGraph)
import qualified SGLR.TableGen.Graph as Graph
import SGLR.TableGen.Automaton (NFA, DFA, LNFA, LDFA, NFATrans(..), AutoState(..))
import qualified SGLR.TableGen.Automaton as Auto
import SGLR.TableGen.Rule (Rule(..), RPart(..), Rule', RPart')
import qualified SGLR.TableGen.Rule as Rule

import qualified SGLR.Model as Model

import Debug.Trace (trace, traceShow)

debug a = traceShow a a

pairWithInput :: (a -> b) -> a -> (a,b)
pairWithInput f i = (i, f i)

sortMapping1 ruleSorts = Map.fromListWith (Set.union) . zip ruleSorts . map Set.singleton
sortMapping2 ruleSorts sortMap f = Map.fromList $ map (pairWithInput (f sortMap)) ruleSorts


get k = Maybe.fromJust . Map.lookup k
splits l = zip (map length $ List.inits l) (List.tails l)

rulesToTables :: (Enum sort, Bounded sort, Ord sort, Enum lit, Bounded lit, Ord lit, Show lit, Show sort)
              => [Rule' sort lit]
              -> sort
              -> a -- (M.InstrTable , M.EOFTable)
rulesToTables rule's startSymbol = error "TODO"
  where rules     = map Rule.fromRule' rule's
        ruleSorts = map Rule.ruleSort rules
        ruleBodys = map Rule.ruleBody rules
        sortMap   = sortMapping1 ruleSorts rules
        sortToRNo = sortMapping1 ruleSorts [0..]
        firstSrts = sortMapping2 ruleSorts sortMap firstSorts -- epsilons between sorts
        firstLits = sortMapping2 ruleSorts sortMap firstLits1 -- transitions from sorts
        firsts    = sortMapping2 ruleSorts sortMap firstLits2
        follows   = followSet firsts rules startSymbol
        dfa = Auto.toDfa (nfa rules startSymbol sortToRNo)

nfa :: (Enum sort, Bounded sort, Ord sort, Enum lit, Bounded lit, Ord lit, Show lit, Show sort)
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
        initials = map fromIntegral $ List.elemIndices startSymbol $ map Rule.ruleSort rules
        fakeRule = StartRule startSymbol
        addAccept (n,g) = (n+1, Graph.lAddEdges (map (\from -> (from,n+1,NFATrans $ Srt startSymbol)) initials) $ Graph.lAddNodes [(n+1, Final (1,fakeRule))] g)
        addInitial (n,g) = Graph.lAddEdges (map (\to -> (n+1,to,Eps)) initials) $ Graph.lAddNodes [(n+1, Initial (0,fakeRule))] g

firstLits1 :: (Ord lit, Ord sort)
            => Map sort (Set (Rule sort lit))
            -> sort 
            -> Set lit
firstLits1 sortMap sort = Set.map Rule.fromLit lits
  where lits = Set.filter Rule.isLit $ Set.map (head . Rule.ruleBody) $ get sort sortMap

firstSorts :: (Ord lit, Ord sort)
           => Map sort (Set (Rule sort lit))
           -> sort 
           -> Set sort
firstSorts sortMap sort = Set.filter (/= sort) $ Set.map Rule.fromSrt sorts
  where sorts = Set.filter Rule.isSrt $ Set.map (head . Rule.ruleBody) $ get sort sortMap

firstLits2 :: (Ord lit, Ord sort)
           => Map sort (Set (Rule sort lit))
           -> sort 
           -> Set lit
firstLits2 sortMap sort = Set.union lits' recLits
  where (lits,sorts) = Set.partition Rule.isLit $ Set.map (head . Rule.ruleBody) $ get sort sortMap
        lits' = Set.map Rule.fromLit lits
        recLits = firstLits2 sortMap =<< (Set.filter (/= sort) $ Set.map Rule.fromSrt sorts)

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

getFollowers s firsts result r = 
  followersToSet firsts result
    $ map (headRightOrLeft rs)                -- get the heads of the follow lists
    $ filter (not . elem (Srt s))             -- drop those that are s
    $ appendEmpty s rs                        -- append empty list if last list consists of s's
    $ dropWhile (not . elem (Srt s))          -- drop the list before the first s
    $ List.groupBy (bothOrNeither (Srt s)) rb -- split into lists of s and not s
  where rs = Rule.ruleSort r
        rb = Rule.ruleBody r

bothOrNeither s a b = (a == s && b == s) || (a /= s && b /= s)
headRightOrLeft k   = maybe (Left k) Right . Maybe.listToMaybe
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
followersToSet firsts followSet eithers = Set.unions
  $ flip map eithers $ \either ->
    case either of
      Left k        -> get k followSet
      Right (Srt s) -> Set.map Lit $ get s firsts
      Right (Lit l) -> Set.singleton (Lit l)
