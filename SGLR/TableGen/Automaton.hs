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
  where g'    = buildDfa initial' initial Graph.lEmpty
        g''   = setInitial $ Graph.lNodeMap (autoNodeSet . Set.map (Maybe.fromJust . Graph.label g)) g'
        psn l = sum $ map (\n -> 2^n) l -- PowerSetNumber
        psn'  = psn . Set.toList
        n     = Graph.nodes g
        e     = Graph.edges g
        closure n = closure' [] n
          where closure' l n = n : (closure' (n:l) =<< (map fst epsE List.\\ (n:l)))
                  where epsE = filter (\(_,l) -> l == Eps) $ Set.toList $ Graph.outE g n
        close n = (psn c, c) where c = closure n
        initial = case map fst $ filter (\(_,l) -> isInitial l) n of
          []    -> error "No initial state found"
          _:_:_ -> error "Multiple initial states found"
          [i]   -> closure i
        initial' = psn initial
        --buildDfa :: s -> [s] -> LGraph (Set s) t -> LGraph (Set s) t
        buildDfa n c gr = if lGraphMember n gr then gr else Graph.lAddEdges es' gr''
          where gr'  = Graph.lAddNode n (Set.fromList c) gr
                es   = (transWrapperToSet . removeEps . Set.toList . Graph.outE g) =<< c
                lmap = Map.toList $ Map.fromListWith Set.union es
                es'  = map (\(l,s) -> (n,psn' s,l)) lmap
                ns   = map (\(_,s) -> let s' = Set.toList s in (psn s', s')) lmap
                gr'' = List.foldl' (\gr''' n -> uncurry buildDfa n gr''') gr' ns
                removeEps = filter (\(_,l) -> l /= Eps)
                transWrapperToSet = map (\(n,NFATrans l) -> (l, Set.singleton n))
        autoNodeSet s = if Set.filter isFinal s /= Set.empty
            then Final s'
            else Normal s'
          where s' = Set.map getLabel s
        --setInitial :: LGraph (AutoState s) t -> LGraph (AutoState s) t
        setInitial gr = case Maybe.fromJust $ Graph.lLabel initial' gr of
          Normal s -> Graph.lAddNode initial' (Initial s) gr
          Final  s -> Graph.lAddNode initial' (IFinal  s) gr
        lGraphMember :: Integer -> LGraph n e -> Bool
        lGraphMember n (ns,_) = List.elem n $ List.map fst ns
