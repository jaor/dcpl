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

module DCPL.PostFix where

import Control.Monad.State

data StackElem = Val Integer | Cmd [Command]

type Command = State [StackElem] ()

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
postfix nArgs action args =
  if length args < nArgs then error "Not enough args"
  else case head (snd (runState action (map Val args))) of
           Val x -> x
           _ -> error "Stack doesn't contain an integer"

postfix' :: Int -> [Command] -> [Integer] -> Integer
postfix' nArgs actions = postfix nArgs (sequence_ actions)

makePostFix :: ([StackElem] -> [StackElem]) -> Command
makePostFix f = State $ \s -> ((), f s)

combine :: [Command] -> Command
combine actions = makePostFix $ \s -> Cmd actions : s

exec :: Command
exec = do
  s <- get
  case s of
    Cmd actions:_ -> pop >> (sequence_ actions)
    Val x:_ -> error $ "Cannot apply exec to value " ++ show x
    _ -> error "Empty stack"

push :: Integer -> Command
push n = makePostFix $ \s -> Val n : s

pop :: Command
pop = makePostFix tail

swap :: Command
swap = makePostFix f
  where f (a:b:es) = b:a:es
        f _ = error "Stack underflow"

nget :: Command
nget = makePostFix f
  where f (Val n:es) = nth n es:es
        f _ = error "Cannot apply nget to non-number"
        nth n _ | n <= 0 = error $ "sel: bad index (" ++ show n ++ ")"
        nth _ [] = error "sel: bad index"
        nth 0 (e:_) = e
        nth n (_:es) = nth (n - 1) es

makeArith :: (Integer -> Integer -> Integer) -> String -> Command
makeArith op name = makePostFix f
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
sel = makePostFix f
  where f s | length s < 3 = error "Not enough args for sel"
        f (z:o:Val x:es) = (if x == 0 then z else o) : es
        f _ = error "Cannot apply sel to non-number condition"
