module SGLR.TableGen.Automaton where

import SGLR.TableGen.Graph (Graph, LGraph)
import qualified SGLR.TableGen.Graph as Graph

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Word (Word)

import Debug.Trace (trace, traceShow)

debug a = traceShow a a

data NFATrans label = Eps            -- ^ Epsilon
                    | NFATrans label -- ^ wrapper
                    deriving (Eq, Ord, Show)
data AutoState label = Normal  label
                     | Initial label
                     | Final   label
                     | IFinal  label
                     deriving (Eq, Ord, Show)

isInitial an = case an of
  Normal  _ -> False
  Initial _ -> True
  Final   _ -> False
  IFinal  _ -> True

isFinal an = case an of
  Normal  _ -> False
  Initial _ -> False
  Final   _ -> True
  IFinal  _ -> True

getLabel an = case an of
  Normal  l -> l
  Initial l -> l
  Final   l -> l
  IFinal  l -> l

type DFA s t = Graph (AutoState s) t
type NFA s t = DFA s (NFATrans t)

type LDFA s t = LGraph (AutoState s) t
type LNFA s t = LDFA s (NFATrans t)

toDfa :: (Ord s, Ord t, Eq t, Show s, Show t)
      => NFA s t
      -> DFA (Set s) t
toDfa g = Graph.renumber $ Graph.fromLGraph g''
  where g'    = buildDfa g initial' initial Graph.lEmpty
        g''   = setInitial initial' $ Graph.lNodeMap (setToNode . Set.map (Maybe.fromJust . Graph.label g)) g'
        initial  = closure g (findInitial g)
        initial' = psn initial

setToNode :: (Ord l) => Set (AutoState l) -> AutoState (Set l)
setToNode s = if Set.filter isFinal s /= Set.empty
    then Final s'
    else Normal s'
  where s' = Set.map getLabel s

findInitial :: NFA s t -> Integer
findInitial g = case map fst $ filter (\(_,l) -> isInitial l) (Graph.nodes g) of
  []    -> error "No initial state found"
  _:_:_ -> error "Multiple initial states found"
  [i]   -> i

closure :: (Eq t) => NFA s t -> Integer -> [Integer]
closure g n = closure' [] n -- Note the `List.\\ l'`, to stop the recursion!
  where closure' l m = m : (closure' l' =<< (eps List.\\ l'))
          where l' = m:l
                eps = map fst $ filter isEpsAdj $ Set.toList $ Graph.outE g m

isEpsAdj :: (Eq l) => Graph.Adj (NFATrans l) -> Bool
isEpsAdj (_,l) = l == Eps

psn l = sum $ map (\n -> 2^n) l -- PowerSetNumber
psn'  = psn . Set.toList
close g n = (psn c, c) where c = closure g n

setInitial :: Integer -> LDFA s t -> LDFA s t
setInitial initial' gr = case Maybe.fromJust $ Graph.lLabel initial' gr of
  Normal s -> Graph.lAddNode initial' (Initial s) gr
  Final  s -> Graph.lAddNode initial' (IFinal  s) gr

buildDfa :: (Ord t, Eq t) => NFA s t -> Integer -> [Integer] -> LGraph (Set Integer) t -> LGraph (Set Integer) t
buildDfa g n c gr = if Graph.lMember n gr then gr else Graph.lAddEdges es' gr''
  where gr'  = Graph.lAddNode n (Set.fromList c) gr
        es   = (transWrapperToSet . filter (not . isEpsAdj) . Set.toList . Graph.outE g) =<< c
        lmap = Map.toList $ Map.fromListWith Set.union es
        es'  = map (\(l,s) -> (n,psn' s,l)) lmap
        ns   = map (\(_,s) -> let s' = Set.toList s in (psn s', s')) lmap
        gr'' = List.foldl' (flip $ uncurry $ buildDfa g) gr' ns
        transWrapperToSet = map (\(n,NFATrans l) -> (l, Set.singleton n))
