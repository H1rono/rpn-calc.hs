{-# LANGUAGE InstanceSigs #-}

module Rpn (
    Input,
    Output,
    Operator,
    Token,
    Stack,
    UnexpectedToken,
    pushToken,
    solve,
) where

import qualified Control.Exception as E

type Input = String
type Output = Double

data Operator = Add | Sub | Mul | Div

instance Show Operator where
    showsPrec :: Int -> Operator -> ShowS
    showsPrec d Add = showsPrec d "+"
    showsPrec d Sub = showsPrec d "-"
    showsPrec d Mul = showsPrec d "*"
    showsPrec d Div = showsPrec d "/"

instance Read Operator where
    readsPrec :: Int -> ReadS Operator
    readsPrec _ ('+' : s) = [(Add, s)]
    readsPrec _ ('-' : s) = [(Sub, s)]
    readsPrec _ ('*' : s) = [(Mul, s)]
    readsPrec _ ('/' : s) = [(Div, s)]
    readsPrec _ _ = []

data Token = V Double | Op Operator

instance Show Token where
    showsPrec :: Int -> Token -> ShowS
    showsPrec d (V v) = showsPrec d v
    showsPrec d (Op op) = showsPrec d op

instance Read Token where
    readsPrec :: Int -> ReadS Token
    readsPrec d s = vPrec ++ opPrec
      where
        vPrec = do
            (v, s') <- (readsPrec d :: ReadS Double) s
            pure (V v, s')
        opPrec = do
            (op, s') <- (readsPrec d :: ReadS Operator) s
            pure (Op op, s')

type Stack = [Double]

newtype UnexpectedToken = UnexpectedToken Token

instance Show UnexpectedToken where
    showsPrec :: Int -> UnexpectedToken -> ShowS
    showsPrec d (UnexpectedToken t) = showsPrec d e
      where
        e = "unexpected token " ++ show t

instance E.Exception UnexpectedToken

pushToken :: Stack -> Token -> IO Stack
pushToken s (V v) = return $ s ++ [v]
pushToken s t@(Op op) = do
    (sInit, x, y) <- splitted
    return $ sInit ++ [x `op'` y]
  where
    splitted = case reverse s of
        (y : x : sInitRev) -> pure (reverse sInitRev, x, y)
        _ -> E.throwIO $ UnexpectedToken t
    op' = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> (/)

-- TODO
solve :: Input -> Output
solve _ = 0.0
