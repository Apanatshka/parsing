import qualified Data.WordMap.Strict as WordMap
import qualified Data.Array.IArray as A
import Data.Array.IArray ((!))
import qualified Data.Maybe as Maybe
import Debug.Trace (traceShow)

import qualified SGLR.Model as M
import SGLR.Model (Instruction(..), StackElem(..))
import qualified Data.ByteString.Lazy as B
import Data.Word

import SGLR.Engine (runParser)

-- grammar 3.1 from "modern compiler implementaion in Java" second edition, by Andrew W. Appel

buildTable l = A.listArray (0, (fromIntegral $ length l') - 1) l' where l' = map WordMap.fromList l
enumToIntegral :: (Enum a, Num c) => a -> c
enumToIntegral = fromIntegral . fromEnum

data Input = Id
           | Num
           | Prnt
           | Semicolon
           | Cmma
           | Pls
           | ColonEq
           | ParensOpen
           | ParensClose
           deriving (Eq, Ord, Show, Enum)
data Sort = S | E | L deriving (Eq, Ord, Show, Enum)

data AST = Seq AST AST
         | Assign AST
         | Print AST
         | IdWrap
         | NumWrap
         | Plus AST AST
         | Tuple AST AST
         | Comma AST AST
         deriving (Eq, Ord, Show)

input :: B.ByteString
input = B.pack $ map enumToIntegral [Id {-a-}, ColonEq, Num {-7-}, Semicolon, Id {-b-}, ColonEq, Id {-c-}, Pls, ParensOpen, Id {-d-}, ColonEq, Num {-5-}, Pls, Num {-6-}, Cmma, Id {-d-}, ParensClose]


rule :: A.Array Int (M.Rule AST)
rule = (\l -> A.listArray (0,length l - 1) l)
  [ (3, gotoTable ! (enumToIntegral S), \[Trm (_,e2), _, Trm (_,e1)] -> (enumToIntegral S, Seq e1 e2))     -- S = S ";" S
  , (3, gotoTable ! (enumToIntegral S), \[Trm (_,e), _, _] -> (enumToIntegral S, Assign e))                -- S = id ":=" E
  , (4, gotoTable ! (enumToIntegral S), \[_, Trm (_,l), _, _] -> (enumToIntegral S, Print l))              -- S = "print" "(" L ")"
  , (1, gotoTable ! (enumToIntegral E), \[_] -> (enumToIntegral E, IdWrap))                                -- E = id
  , (1, gotoTable ! (enumToIntegral E), \[_] -> (enumToIntegral E, NumWrap))                               -- E = num
  , (3, gotoTable ! (enumToIntegral E), \[Trm (_,e2), _, Trm (_,e1)] -> (enumToIntegral E, Plus e1 e2))    -- E = E "+" E
  , (5, gotoTable ! (enumToIntegral E), \[_, Trm (_,e), _, Trm (_,s), _] -> (enumToIntegral E, Tuple s e)) -- E = "(" S "," E ")"
  , (1, gotoTable ! (enumToIntegral L), \[Trm (_,e)] -> (enumToIntegral E, e))                             -- L = E
  , (3, gotoTable ! (enumToIntegral L), \[Trm (_,e), _, Trm (_,l)] -> (enumToIntegral L, Comma l e))       -- L = L "," E
  ]

shift  n = Shift  $ (n-1,instrTable ! (n-1))
reduce k = Reduce $ rule            ! (k-1)

instrTable :: M.InstrTable AST
instrTable = buildTable $ map (map (\(a,b) -> (enumToIntegral a,b)))
  [ [ (Id,   shift 4)           -- 0
    , (Prnt, shift 7)
    ]
  , [ (Semicolon, shift 3) ]    -- 1
  , [ (Id,   shift 4)           -- 2
    , (Prnt, shift 7)
    ]
  , [ (ColonEq, shift 6) ]      -- 3
  , [ (Semicolon, reduce 1)     -- 4
    , (Cmma,      reduce 1)
    ]
  , [ (Id,         shift 20)    -- 5
    , (Num,        shift 10)
    , (ParensOpen, shift 8)
    ]
  , [ (ParensOpen, shift 9) ]   -- 6
  , [ (Id,   shift 4)           -- 7
    , (Prnt, shift 7)
    ]
  , [ (Id,         shift 20)    -- 8
    , (Num,        shift 10)
    , (ParensOpen, shift 8)
    ]
  , [ (Semicolon,   reduce 5)   -- 9
    , (Cmma,        reduce 5)
    , (Pls,         reduce 5)
    , (ParensClose, reduce 5)
    ]
  , [ (Semicolon, reduce 2)     -- 10
    , (Cmma,      reduce 2)
    , (Pls,       shift 16)
    ]
  , [ (Semicolon, shift 3)      -- 11
    , (Cmma,      shift 18)
    ]
  , [ (Semicolon, reduce 3)     -- 12
    , (Cmma,      reduce 3)
    ]
  , [ (Cmma,        shift 19)   -- 13
    , (ParensClose, shift 13)
    ]
  , [ (Cmma,        reduce 8)   -- 14
    , (ParensClose, reduce 8)
    ]
  , [ (Id,         shift 20)    -- 15
    , (Num,        shift 10)
    , (ParensOpen, shift 8)
    ]
  , [ (Semicolon,   reduce 6)   -- 16
    , (Cmma,        reduce 6)
    , (Pls,         shift 16)
    , (ParensClose, reduce 6)
    ]
  , [ (Id,         shift 20)    -- 17
    , (Num,        shift 10)
    , (ParensOpen, shift 8)
    ]
  , [ (Id,         shift 20)    -- 18
    , (Num,        shift 10)
    , (ParensOpen, shift 8)
    ]
  , [ (Semicolon,   reduce 4)   -- 19
    , (Cmma,        reduce 4)
    , (Pls,         reduce 4)
    , (ParensClose, reduce 4)
    ]
  , [ (ParensClose, shift 22) ] -- 20
  , [ (Semicolon,   reduce 7)   -- 21
    , (Cmma,        reduce 7)
    , (Pls,         reduce 7)
    , (ParensClose, reduce 7)
    ]
  , [ (Cmma,        reduce 9)   -- 22
    , (Pls,         shift 16)
    , (ParensClose, reduce 9)
    ]
  ]

gotoTable :: M.GotoTable AST
gotoTable = buildTable $ map (map (\(a,b) -> (a-1,(b-1,instrTable ! (b-1)))))
  [ [(1,2), (3,5), (8,12)]                      -- S
  , [(6,11), (9,15), (16,17), (18,21), (19,23)] -- E
  , [(9,14)]                                    -- L
  ]

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

main = print $ runParser instrTable eofTable input