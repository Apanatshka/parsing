module SGLR.TableGen.Graph where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

-- | Data.Graph from both the containers and the graph-core package
--   don't support labels for nodes *and* edges. fgl /seems/ to have a
--   bug where some edges aren't added (in both PatriciaTree and Tree
--   implementations). So screw it, I'm rolling my own basic Graph. 
-- | This small graph lib is not tuned for performance. It bases some
--   names of fgl. The Graph type is query-only. The LGraph type is for
--   building the graph. 

type IntegerMap = Map Integer

type Graph nodeLabel edgeLabel = (IntegerMap nodeLabel, IntegerMap (Set (Adj edgeLabel)), IntegerMap (Set (Adj edgeLabel)))
type Node nodeLabel = (Integer, nodeLabel)
type Edge edgeLabel = (Integer, Integer, edgeLabel)
type Adj edgeLabel = (Integer, edgeLabel)

nodes :: Graph n e -> [Node n]
nodes (nodeMap, _, _) = Map.assocs nodeMap

edges :: Graph n e -> [Edge e]
edges (_, forwardMap, _) = unpack =<< Map.assocs forwardMap
  where unpack (from,set) = map (\(to,label) -> (from,to,label)) $ Set.toList set

graph :: Ord e => [Node n] -> [Edge e] -> Graph n e
graph ns es = (nodeMap, toMap fwdE es', toMap bwdE es')
  where nodeMap = Map.fromList ns
        fwdE (f,t,l) = (f, Set.singleton (t,l))
        bwdE (f,t,l) = (t, Set.singleton (f,l))
        toMap f = Map.fromListWith Set.union . map f
        es' = filter (\(f,t,_) -> Map.member f nodeMap && Map.member t nodeMap) es

fromLGraph :: Ord e => LGraph n e -> Graph n e
fromLGraph = uncurry graph

empty :: Graph n e
empty = (Map.empty, Map.empty, Map.empty)

outE :: Graph n e -> Integer -> Set (Adj e)
outE (_, fwdMap, _) nodeId = Map.findWithDefault Set.empty nodeId fwdMap

inE :: Graph n e -> Integer -> Set (Adj e)
inE (_, _, bwdMap) nodeId = Map.findWithDefault Set.empty nodeId bwdMap

label :: Graph n e -> Integer -> Maybe n
label (nodeMap, _, _) n = Map.lookup n nodeMap

nmap :: Graph n e -> (n -> n') -> Graph n' e
nmap (nodeMap, fwd, bwd) f = (Map.map f nodeMap, fwd, bwd)

prettify :: (Show n, Show e) => Graph n e -> String
prettify g@(nodeMap, _, _) = pretty =<< ns
  where ns = Map.assocs nodeMap
        pretty (n,l) =  show n ++ ". " 
                     ++ show l ++ ": " 
                     ++ show (Set.toList $ outE g n) ++ "\n"

renumber :: (Ord e) =>  Graph n e -> Graph n e
renumber (nodeMap, fwdMap, bwdMap) = (nodeMap', fwdMap', bwdMap')
  where renumberMap = Map.fromList $ zip (Map.keys nodeMap) [0..]
        mapKeys  = Map.mapKeysMonotonic (renumberMap Map.!)
        mapAdj   = Map.map $ Set.map $ \(adj,l) -> (renumberMap Map.! adj, l)
        nodeMap' = mapKeys nodeMap
        fwdMap'  = mapAdj $ mapKeys fwdMap
        bwdMap'  = mapAdj $ mapKeys bwdMap

type LGraph n e = ([Node n], [Edge e])

-- adding to the end of the list makes sure that when fromLGraph is
-- used, the newest added nodes/edges become part of the graph. 

lAddNodes :: [Node n] -> LGraph n e -> LGraph n e
lAddNodes l (ns,es) = (ns ++ l, es)

lAddEdges :: [Edge e] -> LGraph n e -> LGraph n e
lAddEdges l (ns,es) = (ns, es ++ l)

lNodeMap :: (n -> n') -> LGraph n e -> LGraph n' e
lNodeMap  f (ns,es) = (map (\(n,l) -> (n,f l)) ns, es)

lEmpty :: LGraph n e
lEmpty = ([],[])

lAddNode :: Integer -> n -> LGraph n e -> LGraph n e
lAddNode n l = lAddNodes [(n,l)]

lAddEdge :: Integer -> Integer -> e -> LGraph n e -> LGraph n e
lAddEdge f t l = lAddEdges [(f,t,l)]

lLabel :: Integer -> LGraph n e -> Maybe n
lLabel n (ns,_) = List.lookup n ns
