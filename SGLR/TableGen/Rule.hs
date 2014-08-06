module SGLR.TableGen.Rule (
  RPart(..)
, isLit
, isSrt
, fromLit
, fromSrt
, Rule(..)
, ruleSort
, ruleBody
, ruleCons
, ruleSize
, RPart'
, Rule'
, srt
, lit
, cons
, isA
, fromRPart'
, fromRule'
) where

import qualified Data.List as List

-- | Part of a rule, distinhuishing sorts from literals
data RPart sort literal = Srt sort    -- ^ sort (e.g. S)
                        | Lit literal -- ^ literal (e.g. '+')
                        | EOF         -- ^ the End Of a File ($)
                        deriving (Eq, Ord)

instance (Show sort, Show lit) => Show (RPart sort lit) where
  show (Srt sort)    = show sort
  show (Lit literal) = "'" ++ show literal ++ "'"
  show EOF = "<EOF>"

-- check if Lit(eral)
isLit :: RPart a b -> Bool
isLit part = case part of
  Lit _ -> True
  _ -> False

-- check if Sort (Srt)
isSrt :: RPart a b -> Bool
isSrt part = case part of
  Srt _ -> True
  _ -> False

-- unpack Lit, should only be used when you're sure it's a Lit
fromLit :: RPart s l -> l
fromLit (Lit l) = l
fromLit _ = error "fromLit"

-- unpack Srt, should only be used when you're sure it's a Srt
fromSrt :: RPart s l -> s
fromSrt (Srt s) = s
fromSrt _ = error "fromSrt"

-- | A (BNF-like) rule, augmented with a constructor name
data Rule sort lit = Cons sort [RPart sort lit] String -- ^ normal rule with constructor name
                   | IsA  sort sort                    -- ^ sort alias without constructor
                   | StartRule sort                    -- ^ The rule S' -> S <EOF>
                   deriving (Eq, Ord)

instance (Show sort, Show lit) => Show (Rule sort lit) where
  show (Cons s p c)  = show s ++ " -> " ++ List.intercalate " " (map show p) ++ " {" ++ show c ++ "}"
  show (IsA  s1 s2)  = show s1 ++ " -> " ++ show s2
  show (StartRule s) = "<START> -> " ++ show s ++ " <EOF>"

-- | unpack rule (without constructor). 
-- Reading the sort of the start-rule is a runtime error!
ruleToPair :: Rule sort lit -> (sort, [RPart sort lit])
ruleToPair r = case r of 
  Cons sort parts _ -> (sort, parts)
  IsA  sort part    -> (sort, [Srt part])
  StartRule sort    -> (error "Sort <START> has no computable representation", [Srt sort, EOF])

-- | get the sort of a rule
ruleSort :: Rule sort lit -> sort
ruleSort = fst . ruleToPair

-- | get the body of a rule
ruleBody :: Rule sort lit -> [RPart sort lit]
ruleBody = snd . ruleToPair

-- | get the constructor of a rule (if there is one)
ruleCons :: Rule sort lit -> Maybe String
ruleCons r = case r of
  Cons _ _ s -> Just s
  _ -> Nothing

-- | get the (generic) length of the body of the rule
ruleSize :: Num i => Rule sort lit -> i
ruleSize = List.genericLength . ruleBody

-- | like RPart, but with a hidden implementation and no EOF
newtype RPart' sort lit = RPart' (RPart sort lit)
-- | like Rule, but with a hidden implementation and using RPart'
newtype Rule'  sort lit = Rule'  (Rule sort lit)

-- transform a RPart' to a RPart
fromRPart' :: RPart' sort lit -> RPart sort lit
fromRPart' (RPart' rpart) = rpart

-- transform a Rule' to a Rule
fromRule' :: Rule' sort lit -> Rule sort lit
fromRule' (Rule' rpart) = rpart

-- build an RPart' Srt
srt :: sort -> RPart' sort lit
srt s = RPart' (Srt s)

-- build an RPart' Lit
lit :: lit -> RPart' sort lit
lit l = RPart' (Lit l)

-- build an Rule' rule with constructor
cons :: sort -> [RPart' sort lit] -> String -> Rule' sort lit
cons sort parts constr = Rule' (Cons sort (map fromRPart' parts) constr)

-- build an Rule' sort alias rule
isA :: sort -> sort -> Rule' sort lit
isA s1 s2 = Rule' (IsA s1 s2)
