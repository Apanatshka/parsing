module SGLR.TableGen {-(
  BNFPart
, BNFRule(..)
, rulesToTables
)-} where

--import qualified SGLR.Model as M
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

import Debug.Trace (trace, traceShow)

debug a = traceShow a a

-- | Part of a rule, distinhuishing sorts from literals
data BNFPart sort literal = Srt sort
                          | Lit literal
                          deriving (Eq, Ord, Show)
-- | A BNF rule, augmented with a constructor name
data BNFRule sort lit = Cons sort [BNFPart sort lit] String -- ^ normal rule with constructor name
                      | IsA  sort sort                      -- ^ sort alias without constructor
                      deriving (Eq, Ord)

instance (Show sort, Show lit) => Show (BNFRule sort lit) where
  show (Cons s p c) = show s ++ " ::= " ++ List.intercalate " " (map show p) ++ " {" ++ show c ++ "}"
  show (IsA  s1 s2) = show s1 ++ " ::= " ++ show s2

isLit :: BNFPart a b -> Bool
isLit part = case part of
  Srt _ -> False
  Lit _ -> True

isSrt :: BNFPart a b -> Bool
isSrt = not . isLit

fromLit :: BNFPart s l -> l
fromLit (Lit l) = l

fromSrt :: BNFPart s l -> s
fromSrt (Srt s) = s

bnfToPair :: BNFRule sort lit -> (sort, [BNFPart sort lit])
bnfToPair r = case r of 
  Cons sort parts _ -> (sort, parts)
  IsA  sort part    -> (sort, [Srt part])

data Lit' lit = Lit' lit | EOF deriving (Eq, Ord, Show)

pairWithInput :: (a -> b) -> a -> (a,b)
pairWithInput f i = (i, f i)

sortMapping1 ruleSorts = Map.fromListWith (Set.union) . zip ruleSorts . map Set.singleton
sortMapping2 ruleSorts sortMap f = Map.fromList $ map (pairWithInput (f sortMap)) ruleSorts


get k = Maybe.fromJust . Map.lookup k
splits l = zip (map length $ List.inits l) (List.tails l)

rulesToTables :: (Enum sort, Bounded sort, Ord sort, Enum lit, Bounded lit, Ord lit, Show lit, Show sort)
              => [BNFRule sort lit]
              -> sort
              -> a -- (M.InstrTable , M.EOFTable)
rulesToTables bnfRules startSymbol = error "TODO"
  where rules      = map bnfToPair bnfRules
        ruleSorts  = map fst rules
        ruleBodies = map snd rules
        sortMap      = sortMapping1 ruleSorts ruleBodies
        sortToRuleNo = sortMapping1 ruleSorts [0..]
        firstSrts = sortMapping2 ruleSorts sortMap firstSorts -- epsilons between sorts
        firstLits = sortMapping2 ruleSorts sortMap firstLits1 -- transitions from sorts
        firsts    = sortMapping2 ruleSorts sortMap firstLits2
        follows   = followSet firsts rules startSymbol
        dfa = Auto.toDfa (nfa rules startSymbol sortToRuleNo)

nfa :: (Enum sort, Bounded sort, Ord sort, Enum lit, Bounded lit, Ord lit, Show lit, Show sort)
    => [(sort, [BNFPart sort lit])]
    -> sort
    -> Map sort (Set Integer)
    -> NFA (Integer, (sort, [BNFPart sort lit])) (BNFPart sort lit)
nfa rules startSymbol sortToRuleNo = Graph.fromLGraph $ addInitial $ addAccept basicGraph
  where basicGraph = List.foldl' (flip addRule) (List.genericLength rules, ([],[])) indexedRules
        indexedRules = (zip [0..] rules)
        addRule (rn,r@(s,rbody)) (n,g) = (n + List.genericLength rbody, addRuleEdges r n rn (addRuleNodes r n rn g))
        addRuleNodes r@(_,rbody) n rn = Graph.lAddNodes ((rn,h):(zip [n..] t))
          where (h:t) = map (Normal . flip (,) r) [0 .. fromIntegral (length rbody)]
        addRuleEdges r@(_,rbody) n sn = Graph.lAddEdges (ruleEdge =<< (zip (sn:[n..]) (zip [n..] rbody)))
        ruleEdge (n1, (n2, part)) = (n1, n2, NFATrans part) : case part of
          Lit _ -> []
          Srt s -> map (\n2' -> (n1,n2',Eps)) $ Set.toList $ get s sortToRuleNo
        initials = map fromIntegral $ List.elemIndices startSymbol $ map fst rules
        fakeRule = (startSymbol, [Srt startSymbol])
        addAccept (n,g) = (n+1, Graph.lAddEdges (map (\from -> (from,n+1,NFATrans $ Srt startSymbol)) initials) $ Graph.lAddNodes [(n+1, Final (1,fakeRule))] g)
        addInitial (n,g) = Graph.lAddEdges (map (\to -> (n+1,to,Eps)) initials) $ Graph.lAddNodes [(n+1, Initial (0,fakeRule))] g

firstLits1 :: (Ord lit, Ord sort)
            => Map sort (Set [BNFPart sort lit]) 
            -> sort 
            -> Set lit
firstLits1 sortMap sort = Set.map fromLit lits
  where lits = Set.filter isLit $ Set.map head $ get sort sortMap

firstSorts :: (Ord lit, Ord sort)
           => Map sort (Set [BNFPart sort lit]) 
           -> sort 
           -> Set sort
firstSorts sortMap sort = Set.filter (/= sort) $ Set.map fromSrt sorts
  where sorts = Set.filter isSrt $ Set.map head $ get sort sortMap

firstLits2 :: (Ord lit, Ord sort)
           => Map sort (Set [BNFPart sort lit]) 
           -> sort 
           -> Set lit
firstLits2 sortMap sort = Set.union lits' recLits
  where (lits,sorts) = Set.partition isLit $ Set.map head $ get sort sortMap
        lits' = Set.map fromLit lits
        recLits = firstLits2 sortMap =<< (Set.filter (/= sort) $ Set.map fromSrt sorts)

followSet :: (Ord lit, Ord sort)
          => Map sort (Set lit)
          -> [(sort, [BNFPart sort lit])]
          -> sort
          -> Map sort (Set (Lit' lit))
followSet firsts rules start = result
  where result = Map.fromList ( (start, Set.singleton EOF)
                              : map f (filter (/= start) $ List.nub $ map fst rules))
        -- f :: (Ord lit, Ord sort) => sort -> (sort, Set (Lit' lit))
        f s = (s, Set.unions $ map (getFollowers s) $ filter (\(a,b) -> elem (Srt s) b) rules)
          where getFollowers s (k, vs) = 
                  followersToSet firsts result
                    $ map (headRightOrLeft k)                 -- get the heads of the follow lists
                    $ filter (not . elem (Srt s))             -- drop those that are s
                    $ appendEmpty s k                         -- append empty list is last list consists of s's
                    $ dropWhile (not . elem (Srt s))          -- drop the list before the first s
                    $ List.groupBy (bothOrNeither (Srt s)) vs -- split into lists of s and not s
        bothOrNeither s a b = (a == s && b == s) || (a /= s && b /= s)
        headRightOrLeft k   = maybe (Left k) Right . Maybe.listToMaybe
        appendEmpty s k l   = if s /= k -- don't add empty list if recursive rule
                                && elem (Srt s) (last l)
                                  then l ++ [[]]
                                  else l

followersToSet :: (Ord lit, Ord sort)
               => Map sort (Set lit)
               -> Map sort (Set (Lit' lit))
               -> [Either sort (BNFPart sort lit)]
               -> Set (Lit' lit)
followersToSet firsts followSet eithers = Set.unions
  $ flip map eithers $ \either ->
    case either of
      Left k        -> get k followSet
      Right (Srt s) -> Set.map Lit' $ get s firsts
      Right (Lit l) -> Set.singleton (Lit' l)
