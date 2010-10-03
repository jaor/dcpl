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


module DCPL.PostFix where

import Control.Monad.State

data StackElem = Val Integer | Cmd [PostFix]

type PostFix = State [StackElem] ()

postfix :: Int -> PostFix -> [Integer] -> Integer
postfix nArgs action args =
  if (length args < nArgs)
  then error "Not enough args"
  else case head (snd (runState action (map Val args))) of
           Val x -> x
           _ -> error "Stack doesn't contain an integer"

postfix' :: Int -> [PostFix] -> [Integer] -> Integer
postfix' nArgs actions = postfix nArgs (sequence_ actions)

makePostFix :: ([StackElem] -> [StackElem]) -> PostFix
makePostFix f = State $ \s -> ((), f s)

combine :: [PostFix] -> PostFix
combine actions = makePostFix $ \s -> Cmd actions : s

exec :: PostFix
exec = do
  s <- get
  case s of
    [] -> error "Empty stack"
    Cmd actions:as -> put as >> sequence_ actions
    Val x:_ -> error $ "Cannot apply exec to value " ++ show x

push :: Integer -> PostFix
push n = makePostFix $ \s -> Val n : s

pop :: PostFix
pop = makePostFix tail

swap :: PostFix
swap = makePostFix f
  where f (a:b:es) = b:a:es
        f _ = error "Stack underflow"

nget :: PostFix
nget = makePostFix f
  where f (Val n:es) = (nth n es):es
        f _ = error "Cannot apply nget to non-number"
        nth n es | n <= 0 =
          error $ "sel: bad index (" ++ show n ++ ")"
        nth _ [] = error "sel: bad index"
        nth 0 (e:es) = e
        nth n (e:es) = nth (n - 1) es

makeArith op name = makePostFix f
  where f [] = error $ "Not enough args in " ++ name
        f [x] = error $ "Not enough args in " ++ name
        f (Val x:Val y:as) = Val (op x y):as
        f _ = error $ name ++ " applied to non-number"

add :: PostFix
add = makeArith (+) "add"

sub :: PostFix
sub = makeArith (-) "sub"

mul :: PostFix
mul = makeArith (*) "mul"

divp :: PostFix
divp = makeArith div "divp"

remp :: PostFix
remp = makeArith rem "remp"

makeCmp op name = makeArith f name
  where f x y = if op x y then 1 else 0

lt :: PostFix
lt = makeCmp (<) "lt"

gt :: PostFix
gt = makeCmp (>) "gt"

eq :: PostFix
eq = makeCmp (==) "eq"

sel :: PostFix
sel = makePostFix f
  where f s | length s < 3 = error "Not enough args for sel"
        f (z:o:Val x:es) = (if x == 0 then z else o) : es
        f _ = error "Cannot apply sel to non-number condition"
