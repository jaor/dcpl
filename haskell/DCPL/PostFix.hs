------------------------------------------------------------------------------
-- |
-- Module: DCPL.PostFix
-- Copyright: (c) 2010 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Oct 03, 2010 17:45
--
--
-- PostFix language (Chapter 1)
--
------------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-}

module DCPL.PostFix (
    Command
  , postfix, postfix'
  , subr, exec
  , push, pop, swap
  , nget, sel, dup
  , add, sub, mul, divp, remp
  , lt, gt, eq
  , sNot, sAnd, sAnd'
  ) where

data StackElem = Val Integer | Subr [Command] deriving Show

type Command = [StackElem] -> [StackElem]

instance Show Command where
  show _ = "<PostFix command>"

instance Eq Command where
  _ == _ = False

instance Num Command where
  (+) = undefined
  (-) = undefined
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = push

postfix :: Int -> Command -> [Integer] -> Integer
postfix nArgs cmd args =
  if length args < nArgs then error "Not enough args"
  else case head $ cmd $ map Val args of
           Val x -> x
           _ -> error "Stack doesn't contain an integer"

postfix' :: Int -> [Command] -> [Integer] -> Integer
postfix' nArgs cmds = postfix nArgs (merge cmds)

merge :: [Command] -> Command
merge [] = id
merge [c] = c
merge (c:cs) = merge cs . c

subr :: [Command] -> Command
subr cmds s = Subr cmds : s

exec :: Command
exec s =
  case s of
    Subr cmds:es -> merge cmds es
    Val x:_ -> error $ "Cannot apply exec to value " ++ show x
    _ -> error "Empty stack"

push :: Integer -> Command
push n s = Val n : s

pop :: Command
pop = tail

swap :: Command
swap (a:b:es) = b:a:es
swap _ = error "Stack underflow"

nget :: Command
nget (Val n:es) = nth n es:es
  where nth m _ | m <= 0 = error $ "nget: bad index (" ++ show m ++ ")"
        nth _ [] = error "nget: bad index"
        nth 0 (e:_) = e
        nth m (_:s) = nth (m - 1) s
nget _ = error "Cannot apply nget to non-number"

dup :: Command
dup (x:xs) = x:x:xs
dup _ = error "Stack underflow"

makeArith :: (Integer -> Integer -> Integer) -> String -> Command
makeArith op name = f
  where f s | length s < 2 = error $ "Not enough args in " ++ name
        f (Val x:Val y:as) = Val (op y x):as
        f _ = error $ name ++ " applied to non-number"

add :: Command
add = makeArith (+) "add"

sub :: Command
sub = makeArith (-) "sub"

mul :: Command
mul = makeArith (*) "mul"

divp :: Command
divp = makeArith div "divp"

remp :: Command
remp = makeArith rem "remp"

makeCmp :: (Integer -> Integer -> Bool) -> String -> Command
makeCmp op name = makeArith f name
  where f x y = if op x y then 1 else 0

lt :: Command
lt = makeCmp (<) "lt"

gt :: Command
gt = makeCmp (>) "gt"

eq :: Command
eq = makeCmp (==) "eq"

sel :: Command
sel s | length s < 3 = error "Not enough args for sel"
sel (z:o:Val x:es) = (if x == 0 then z else o) : es
sel _ = error "Cannot apply sel to non-number condition"

sNot :: Command
sNot = subr [0, 1, sel]

sAnd :: Command
sAnd = subr [mul, 0, eq, sNot, exec]

sAnd' :: Command
sAnd' = subr [sNot, exec, subr [pop, 0], subr [], sel, exec]
