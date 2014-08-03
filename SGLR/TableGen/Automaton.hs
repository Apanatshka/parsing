module SGLR.TableGen.Automaton where

import SGLR.TableGen.Graph (Graph, LGraph)
import qualified SGLR.TableGen.Graph as Graph

import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
--import Data.Map (Map)
import qualified Data.Map.Strict as Map
--import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import qualified Data.List as List
--import Data.Word (Word)

-- import Debug.Trace (trace, traceShow)

--debug a = traceShow a a

data NFATrans label = Eps            -- ^ Epsilon
                    | NFATrans label -- ^ wrapper
                    deriving (Eq, Ord, Show)
data AutoState label = Normal  label
                     | Initial label
                     | Final   label
                     | IFinal  label
                     deriving (Eq, Ord, Show)

isInitial :: AutoState t -> Bool
isInitial an = case an of
  Normal  _ -> False
  Initial _ -> True
  Final   _ -> False
  IFinal  _ -> True

isFinal :: AutoState t -> Bool
isFinal an = case an of
  Normal  _ -> False
  Initial _ -> False
  Final   _ -> True
  IFinal  _ -> True

getLabel :: AutoState t -> t
getLabel an = case an of
  Normal  l -> l
  Initial l -> l
  Final   l -> l
  IFinal  l -> l

type DFA s t = Graph (AutoState s) t
type NFA s t = DFA s (NFATrans t)

type LDFA s t = LGraph (AutoState s) t
type LNFA s t = LDFA s (NFATrans t)

toDfa :: (Ord s, Ord t)
      => NFA s t
      -> DFA (Set s) t
toDfa g = renumber $ Graph.fromLGraph g''
  where g'    = buildDfa g initial' initial Graph.lEmpty
        g''   = setInitial initial' $ Graph.lNodeMap (setToNode . Set.map (Maybe.fromJust . Graph.label g)) g'
        initial  = closure g (findInitial g)
        initial' = psn initial

setToNode :: (Ord l) => Set (AutoState l) -> AutoState (Set l)
setToNode s = if Set.filter isFinal s /= Set.empty
    then Final s'
    else Normal s'
  where s' = Set.map getLabel s

findInitial :: Graph (AutoState t) e -> Integer
findInitial g = case map fst $ filter (\(_,l) -> isInitial l) (Graph.nodes g) of
  []    -> error "No initial state found"
  _:_:_ -> error "Multiple initial states found"
  [i]   -> i

-- | Renumbers the graph, but makes sure the initial state is 0
renumber :: (Ord e) =>  Graph (AutoState t) e -> Graph (AutoState t) e
renumber g@(nodeMap, fwdMap, bwdMap) = (nodeMap', fwdMap', bwdMap')
  where initial  = findInitial g
        renumberMap = Map.fromList $ (initial,0) : zip (List.delete initial $ Map.keys nodeMap) [1..]
        mapKeys  = Map.mapKeys (renumberMap Map.!)
        mapAdj   = Map.map $ Set.map $ \(adj,l) -> (renumberMap Map.! adj, l)
        nodeMap' = mapKeys nodeMap
        fwdMap'  = mapAdj $ mapKeys fwdMap
        bwdMap'  = mapAdj $ mapKeys bwdMap

closure :: Ord t => NFA s t -> Integer -> [Integer]
closure g n = closure' [] n -- Note the `List.\\ l'`, to stop the recursion!
  where closure' l m = m : (closure' l' =<< (eps List.\\ l'))
          where l' = m:l
                eps = map fst $ filter isEpsAdj $ Set.toList $ Graph.outE g m

isEpsAdj :: Graph.Adj (NFATrans l) -> Bool
isEpsAdj (_,l) = case l of
  Eps -> True
  _   -> False

psn :: (Integral b, Num a) => [b] -> a
psn l = sum $ map (\n -> 2^n) l -- PowerSetNumber

psn' :: Set Integer -> Integer
psn' = psn . Set.toList

close :: (Num n, Ord t) => NFA s t -> Integer -> (n,[Integer])
close g n = (psn c, c) where c = closure g n

setInitial :: Integer -> LDFA s t -> LDFA s t
setInitial initial' gr = case Maybe.fromJust $ Graph.lLabel initial' gr of
  Normal s -> Graph.lAddNode initial' (Initial s) gr
  Final  s -> Graph.lAddNode initial' (IFinal  s) gr
  Initial _ -> gr
  IFinal  _ -> gr

-- | Builds a DFA out of an NFA using powerset construction. The state
-- numbers will be consecutive and the initial state will be 0.
buildDfa :: (Ord t, Eq t) => NFA s t -> Integer -> [Integer] -> LGraph (Set Integer) t -> LGraph (Set Integer) t
buildDfa g n c gr = if Graph.lMember n gr then gr else Graph.lAddEdges es gr''
  where gr'  = Graph.lAddNode n (Set.fromList c) gr
        -- map from transitions (t) to Set of adjacent states (s)
        lmap = multiAssocToAssocSet . nonEpsE g =<< c
        es   = map (\(l,s) -> (n,psn' s,l)) lmap
        
        ns   = map (\(_,s) -> let s' = Set.toList s in (psn s', s')) lmap
        gr'' = List.foldl' (flip $ uncurry $ buildDfa g) gr' ns


unTransSwap :: [(a, NFATrans b)] -> [(b,a)]
unTransSwap = map (\(n,NFATrans l) -> (l, n))

multiAssocToAssocSet :: (Ord a, Ord b) => [(a,b)] -> [(a,Set b)]
multiAssocToAssocSet = Map.toList . Map.fromListWith Set.union . map (\(a,b) -> (a,Set.singleton b))

outE :: Ord e => Graph n e -> Integer -> [Graph.Adj e]
outE g = Set.toList . Graph.outE g

nonEpsE :: Ord t => NFA s t -> Integer -> [(t, Integer)]
nonEpsE g = unTransSwap . filter (not . isEpsAdj) . outE g
