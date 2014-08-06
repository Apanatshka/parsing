import SGLR.TableGen
import SGLR.TableGen.Rule (isA, cons, lit, srt)
import qualified SGLR.TableGen.Rule as Rule

data Sort = S | E | T deriving (Eq, Ord, Bounded, Enum, Show)
data Lit = ParensOpen | ParensClose | Plus | N deriving (Eq, Ord, Bounded, Enum, Show)

-- example from wikipedia: https://en.wikipedia.org/wiki/Canonical_LR_parser
rule's :: [Rule.Rule' Sort Lit]
rule's =
  [ isA S E                                              -- ^ S -> E
  , isA E T                                              -- ^ E -> T
  , cons E [lit ParensOpen, srt E, lit ParensClose] "()" -- ^ E -> ( E )  {"()"}
  , cons T [lit N] "T"                                   -- ^ T -> n      {"T"}
  , cons T [lit Plus, srt T] "+"                         -- ^ T -> + T    {"+"}
  , cons T [srt T, lit Plus, lit N] "_+_"                -- ^ T -> T + n  {"_+_"}
  ]

startSymbol :: Sort
startSymbol = S

main :: IO ()
main = print $ rulesToTables rule's startSymbol
