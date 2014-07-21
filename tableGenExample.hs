import SGLR.TableGen
import qualified SGLR.TableGen.Graph as Graph
import qualified SGLR.TableGen.Automaton as Auto

import qualified Data.Set.Monad as Set
import Data.Set.Monad (Set)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.List as List
import qualified Data.Maybe as Maybe

data Sort = S | E | T deriving (Eq, Ord, Bounded, Enum, Show)
data Lit = ParensOpen | ParensClose | Plus | N deriving (Eq, Ord, Bounded, Enum, Show)

-- example from wikipedia: https://en.wikipedia.org/wiki/Canonical_LR_parser
bnfRules =
  [ IsA S E
  , IsA E T
  , Cons E [Lit ParensOpen, Srt E, Lit ParensClose] "()"
  , Cons T [Lit N] "T"
  , Cons T [Lit Plus, Srt T] "+"
  , Cons T [Srt T, Lit Plus, Lit N] "_+_"
  ]

startSymbol = S

main = putStr $ Graph.prettify dfa
  where rules      = map bnfToPair bnfRules
        ruleSorts  = map fst rules
        ruleBodies = map snd rules
        sortMap      = sortMapping1 ruleSorts ruleBodies
        sortToRuleNo = sortMapping1 ruleSorts [0..]
        firstSrts = sortMapping2 ruleSorts sortMap firstSorts -- epsilons between sorts
        firstLits = sortMapping2 ruleSorts sortMap firstLits1 -- transitions from sorts
        firsts    = sortMapping2 ruleSorts sortMap firstLits2
        follows   = followSet firsts rules startSymbol
        dfa       = Auto.toDfa $ nfa rules startSymbol sortToRuleNo
