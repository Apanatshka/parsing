import SGLR.TableGen
import qualified SGLR.TableGen.Graph as Graph
import qualified SGLR.TableGen.Automaton as Auto
import SGLR.TableGen.Rule (isA, cons, lit, srt)
import qualified SGLR.TableGen.Rule as Rule

data Sort = S | E | T deriving (Eq, Ord, Bounded, Enum, Show)
data Lit = ParensOpen | ParensClose | Plus | N deriving (Eq, Ord, Bounded, Enum, Show)

-- example from wikipedia: https://en.wikipedia.org/wiki/Canonical_LR_parser
rule's :: [Rule.Rule' Sort Lit]
rule's =
  [ isA S E
  , isA E T
  , cons E [lit ParensOpen, srt E, lit ParensClose] "()"
  , cons T [lit N] "T"
  , cons T [lit Plus, srt T] "+"
  , cons T [srt T, lit Plus, lit N] "_+_"
  ]

startSymbol :: Sort
startSymbol = S

main :: IO ()
main = putStr $ Graph.prettify dfa
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
