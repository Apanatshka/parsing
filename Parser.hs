import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

-- Model of an LR Parsing Engine

data Input = Id --String
           | Num --String
           | Print
           | Semicolon
           | Comma
           | Plus
           | Assign
           | ParensOpen
           | ParensClose
           | EOF
           deriving (Eq, Ord, Show)
data Sort = S | E | L deriving (Eq, Ord, Show)

data StackElement = Inp Input
                  | Srt Sort
                  deriving (Eq, Ord, Show)

type State = Integer

type Stack = [(State, StackElement)]

type Rule = (Int, [StackElement] -> StackElement)

data Instruction = Shift State
                 | Goto State
                 | Reduce Int
                 | Accept
                 | Error
                 deriving (Eq, Ord, Show)

type Table = Map.Map State (Map.Map StackElement Instruction)

-- LR Parsing Engine

runParser :: Table -> [Input] -> Bool
runParser table input = (\(Left b) -> b) $ reducel parse [(1, Inp EOF)] $ map Inp input
  where 
    reducel :: (a -> b -> Either Bool b) -> b -> [a] -> Either Bool b
    reducel f def = foldl' (\mb a -> (f a) =<< mb) (Right def)
    
    get k = fromJust . Map.lookup k
    get'  = Map.findWithDefault Error
    
    parse :: StackElement -> Stack -> Either Bool Stack
    parse inp stack@((state,_):_) = executeInstr inp stack instr
      where instr = get' inp (get state table)
    
    executeInstr :: StackElement -> Stack -> Instruction -> Either Bool Stack
    executeInstr inp stack instr = case instr of
      Shift s      -> Right $ (s,inp) : stack
      Goto  s      -> Right $ (s,inp) : stack
      Reduce k     -> parse inp =<< parse (f $ map snd (take i stack)) (drop i stack)
        where (i,f) = rule k
      Accept       -> Left True
      Error        -> Left False

-- example from "modern compiler implementaion in Java" second edition, by Andrew W. Appel

input :: [Input]
input = [Id {-a-}, Assign, Num {-7-}, Semicolon, Id {-b-}, Assign, Id {-c-}, Plus, ParensOpen, Id {-d-}, Assign, Num {-5-}, Plus, Num {-6-}, Comma, Id {-d-}, ParensClose, EOF]

rule :: Int -> Rule
rule i = flip (!!) (i-1)
  [ (3, \[Srt S, Inp Semicolon, Srt S] -> Srt S)
  , (3, \[Srt E, Inp Assign, Inp Id] -> Srt S)
  , (4, \[Inp ParensClose, Srt L, Inp ParensOpen, Inp Print] -> Srt S)
  , (1, \[Inp Id] -> Srt E)
  , (1, \[Inp Num] -> Srt E)
  , (3, \[Srt E, Inp Plus, Srt E] -> Srt E)
  , (5, \[Inp ParensClose, Srt E, Inp Comma, Srt S, Inp ParensOpen] -> Srt E)
  , (1, \[Srt L] -> Srt E)
  , (3, \[Srt E, Inp Comma, Srt L] -> Srt E)
  ]

table :: Table
table = Map.fromList $ zip [1..] $ map Map.fromList
  [ [ (Inp Id,    Shift 4)
    , (Inp Print, Shift 7)
    , (Srt S,     Goto 2)
    ]
  , [ (Inp Semicolon, Shift 3)
    , (Inp EOF,       Accept)
    ]
  , [ (Inp Id,    Shift 4)
    , (Inp Print, Shift 7)
    , (Srt S,     Goto 5)
    ]
  , [ (Inp Assign, Shift 6) ]
  , [ (Inp Semicolon, Reduce 1)
    , (Inp Comma,     Reduce 1)
    , (Inp EOF,       Reduce 1)
    ]
  , [ (Inp Id,         Shift 20)
    , (Inp Num,        Shift 10)
    , (Inp ParensOpen, Shift 8)
    , (Srt E,          Goto 11)
    ]
  , [ (Inp ParensOpen, Shift 9) ]
  , [ (Inp Id,    Shift 4)
    , (Inp Print, Shift 7)
    , (Srt S,     Goto 12)
    ]
  , [ (Inp Id,         Shift 20)
    , (Inp Num,        Shift 10)
    , (Inp ParensOpen, Shift 8)
    , (Srt E,          Goto 15)
    , (Srt L,          Goto 14)
    ]
  , [ (Inp Semicolon,   Reduce 5)
    , (Inp Comma,       Reduce 5)
    , (Inp Plus,        Reduce 5)
    , (Inp ParensClose, Reduce 5)
    , (Inp EOF,         Reduce 5)
    ]
  , [ (Inp Semicolon, Reduce 2)
    , (Inp Comma,     Reduce 2)
    , (Inp Plus,      Shift 16)
    , (Inp EOF,       Reduce 2)
    ]
  , [ (Inp Semicolon, Shift 3)
    , (Inp Comma,     Shift 18)
    ]
  , [ (Inp Semicolon, Reduce 3)
    , (Inp Comma,     Reduce 3)
    , (Inp EOF,       Reduce 3)
    ]
  , [ (Inp Comma,       Shift 19)
    , (Inp ParensClose, Shift 13)
    ]
  , [ (Inp Comma,       Reduce 8)
    , (Inp ParensClose, Reduce 8)
    ]
  , [ (Inp Id,         Shift 20)
    , (Inp Num,        Shift 10)
    , (Inp ParensOpen, Shift 8)
    , (Srt E,          Goto 17)
    ]
  , [ (Inp Semicolon,   Reduce 6)
    , (Inp Comma,       Reduce 6)
    , (Inp Plus,        Shift 16)
    , (Inp ParensClose, Reduce 6)
    , (Inp EOF,         Reduce 6)
    ]
  , [ (Inp Id,         Shift 20)
    , (Inp Num,        Shift 10)
    , (Inp ParensOpen, Shift 8)
    , (Srt E,          Goto 21)
    ]
  , [ (Inp Id,         Shift 20)
    , (Inp Num,        Shift 10)
    , (Inp ParensOpen, Shift 8)
    , (Srt E,          Goto 23)
    ]
  , [ (Inp Semicolon,   Reduce 4)
    , (Inp Comma,       Reduce 4)
    , (Inp Plus,        Reduce 4)
    , (Inp ParensClose, Reduce 4)
    , (Inp EOF,         Reduce 4)
    ]
  , [ (Inp ParensClose, Shift 22) ]
  , [ (Inp Semicolon,   Reduce 7)
    , (Inp Comma,       Reduce 7)
    , (Inp Plus,        Reduce 7)
    , (Inp ParensClose, Reduce 7)
    , (Inp EOF,         Reduce 7)
    ]
  , [ (Inp Comma,       Reduce 9)
    , (Inp Plus,        Shift 16)
    , (Inp ParensClose, Reduce 9)
    ]
  ]

main = print $ runParser table input
