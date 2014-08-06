import qualified Data.WordMap.Strict as WordMap
import qualified Data.Array.IArray as A
import Data.Array.IArray ((!))

import qualified SGLR.Model as M
--import qualified SGLR.Model.Binary as BM
import SGLR.Model (Instruction(..), StackElem(..), State)
import qualified Data.ByteString.Lazy as B
import Data.Word
import Data.List (genericLength)

import SGLR.Engine (runParser)

-- grammar 3.1 from "modern compiler implementation in Java" second edition, by Andrew W. Appel

-- | create a zero-index array from a list, first mapping a function over it
buildTable :: (Num i, A.Ix i, A.IArray arr e) => (a -> e) -> [a] -> arr i e
buildTable f l = A.listArray (0, (genericLength l') - 1) l' where l' = map f l

-- | Turn an enumerable into a number (the standard fromEnum transforms to Int :( )
enumToIntegral :: (Enum a, Num c) => a -> c
enumToIntegral = fromIntegral . fromEnum

-- | input tokens
data Input = Id          -- ^ program identifier
           | Num         -- ^ number
           | Prnt        -- ^ 'print' (keyword)
           | Semicolon   -- ^ ';'
           | Cmma        -- ^ ','
           | Pls         -- ^ '+'
           | ColonEq     -- ^ ':='
           | ParensOpen  -- ^ '('
           | ParensClose -- ^ ')'
           deriving (Eq, Ord, Show, Enum)
-- | sorts of the grammar
data Sort = S | E | L deriving (Eq, Ord, Show, Enum)

-- | the AST to be produced by the parser
data AST = Seq AST AST
         | Assign AST
         | Print AST
         | IdWrap
         | NumWrap
         | Plus AST AST
         | Tuple AST AST
         | Comma AST AST
         deriving (Eq, Ord, Show)

-- example input, turned into Bytes and packed into a ByteString
input :: B.ByteString
input = B.pack $ map enumToIntegral [Id, ColonEq, Num, Semicolon, Id, ColonEq, Id, Pls, ParensOpen, Id, ColonEq, Num, Pls, Num, Cmma, Id, ParensClose]
--                                   a   :=       7    ;          b   :=       c   +    (           d   :=       5    +    6    ,     d   )

-- the rules of the grammar
-- the first number is the number of things to pop of the stack
-- the second thing is the sort (or the number of it in the Enum)
-- the third thing is the actual action, a function that matches the body of the rule to produce the AST
--  It matches the top part of the stack (notice the reversed order in comparison with the rules in the comments
rule :: A.Array Int (M.Rule AST)
rule = buildTable id
  [ (3, s, \[Trm (_,e2), _, Trm (_,e1)]     -> (s, Seq e1 e2))  -- ^ S = S ";" S
  , (3, s, \[Trm (_,e), _, _]               -> (s, Assign e))   -- ^ S = id ":=" E
  , (4, s, \[_, Trm (_,l), _, _]            -> (s, Print l))    -- ^ S = "print" "(" L ")"
  , (1, e, \[_]                             -> (e, IdWrap))     -- ^ E = id
  , (1, e, \[_]                             -> (e, NumWrap))    -- ^ E = num
  , (3, e, \[Trm (_,e2), _, Trm (_,e1)]     -> (e, Plus e1 e2)) -- ^ E = E "+" E
  , (5, e, \[_, Trm (_,e), _, Trm (_,s), _] -> (e, Tuple s e))  -- ^ E = "(" S "," E ")"
  , (1, e, \[Trm (_,e)]                     -> (e, e))          -- ^ L = E
  , (3, l, \[Trm (_,e), _, Trm (_,l)]       -> (l, Comma l e))  -- ^ L = L "," E
  ]
where s = enumToIntegral S
      e = enumToIntegral E
      l = enumToIntegral L

-- build a state from the statenumber by resolving it in the different tables
state :: Word -> State AST
state n = (instrTable ! (n-1), gotoTable ! (n-1), WordMap.lookup (n-1) eofTable)

-- create a shift instruction
shift :: Word -> Instruction AST
shift  n = Shift $ state n

-- create a reduce instruction (resolve the rule number)
reduce :: Int -> Instruction AST
reduce k = Reduce $ rule ! (k-1)

-- create a goto "instruction"
goto :: Word -> State AST
goto = state

-- create the instruction table from a nested list of association pairs (input, instruction)
instrTable :: M.InstrTable AST
instrTable = buildTable WordMap.fromList $ map (map (\(a,b) -> (enumToIntegral a,b)))
  [ [ (Id,   shift 4)           -- state number 0
    , (Prnt, shift 7)
    ]
  , [ (Semicolon, shift 3) ]    -- state number 1
  , [ (Id,   shift 4)           -- state number 2
    , (Prnt, shift 7)
    ]
  , [ (ColonEq, shift 6) ]      -- state number 3
  , [ (Semicolon, reduce 1)     -- state number 4
    , (Cmma,      reduce 1)
    ]
  , [ (Id,         shift 20)    -- state number 5
    , (Num,        shift 10)
    , (ParensOpen, shift 8)
    ]
  , [ (ParensOpen, shift 9) ]   -- state number 6
  , [ (Id,   shift 4)           -- state number 7
    , (Prnt, shift 7)
    ]
  , [ (Id,         shift 20)    -- state number 8
    , (Num,        shift 10)
    , (ParensOpen, shift 8)
    ]
  , [ (Semicolon,   reduce 5)   -- state number 9
    , (Cmma,        reduce 5)
    , (Pls,         reduce 5)
    , (ParensClose, reduce 5)
    ]
  , [ (Semicolon, reduce 2)     -- state number 10
    , (Cmma,      reduce 2)
    , (Pls,       shift 16)
    ]
  , [ (Semicolon, shift 3)      -- state number 11
    , (Cmma,      shift 18)
    ]
  , [ (Semicolon, reduce 3)     -- state number 12
    , (Cmma,      reduce 3)
    ]
  , [ (Cmma,        shift 19)   -- state number 13
    , (ParensClose, shift 13)
    ]
  , [ (Cmma,        reduce 8)   -- state number 14
    , (ParensClose, reduce 8)
    ]
  , [ (Id,         shift 20)    -- state number 15
    , (Num,        shift 10)
    , (ParensOpen, shift 8)
    ]
  , [ (Semicolon,   reduce 6)   -- state number 16
    , (Cmma,        reduce 6)
    , (Pls,         shift 16)
    , (ParensClose, reduce 6)
    ]
  , [ (Id,         shift 20)    -- state number 17
    , (Num,        shift 10)
    , (ParensOpen, shift 8)
    ]
  , [ (Id,         shift 20)    -- state number 18
    , (Num,        shift 10)
    , (ParensOpen, shift 8)
    ]
  , [ (Semicolon,   reduce 4)   -- state number 19
    , (Cmma,        reduce 4)
    , (Pls,         reduce 4)
    , (ParensClose, reduce 4)
    ]
  , [ (ParensClose, shift 22) ] -- state number 20
  , [ (Semicolon,   reduce 7)   -- state number 21
    , (Cmma,        reduce 7)
    , (Pls,         reduce 7)
    , (ParensClose, reduce 7)
    ]
  , [ (Cmma,        reduce 9)   -- state number 22
    , (Pls,         shift 16)
    , (ParensClose, reduce 9)
    ]
  ]

-- create the goto table from a nested list of association pairs (sort, state)
gotoTable :: M.GotoTable AST
gotoTable = buildTable (M.Goto . WordMap.fromList) $ map (map (\(a,b) -> (enumToIntegral a, goto b)))
  [ [(S, 2)]         -- state number 1
  , []               -- state number 2
  , [(S, 5)]         -- state number 3
  , []               -- state number 4
  , []               -- state number 5
  , [(E,11)]         -- state number 6
  , []               -- state number 7
  , [(S,12)]         -- state number 8
  , [(E,15), (L,14)] -- state number 9
  , []               -- state number 10
  , []               -- state number 11
  , []               -- state number 12
  , []               -- state number 13
  , []               -- state number 14
  , []               -- state number 15
  , [(E,17)]         -- state number 16
  , []               -- state number 17
  , [(E,21)]         -- state number 18
  , [(E,23)]         -- state number 19
  , []               -- state number 20
  , []               -- state number 21
  , []               -- state number 22
  ]

-- create the EOF table from a nested list of association pairs (state, instruction)
eofTable :: M.EOFTable AST
eofTable = WordMap.fromList $ map (\(a,b) -> (a-1,b))
  [ (2,  Accept)
  , (5,  reduce 1)
  , (10, reduce 5)
  , (11, reduce 2)
  , (13, reduce 3)
  , (17, reduce 6)
  , (20, reduce 4)
  , (22, reduce 7)
  ]

-- print the output of the parser on these tables and the input
-- result of the parser is of type `Maybe AST`, denoting that parser might fail when the input is invalid
main :: IO ()
main = print $ runParser instrTable gotoTable eofTable input
