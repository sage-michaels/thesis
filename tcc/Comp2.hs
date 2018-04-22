{-# LANGUAGE TupleSections #-}

module Examples.Comp2 where

import Circuit
import Circuit.Builder
import Control.Monad

export :: [(String, [IO (String, Acirc)])] 
export = [("comp2_4", [return ("comp2_4", comp2 8)])] 
--change expected input size in the above line
comp2 :: Int -> Circuit ArithGate
comp2 linputs = buildCircuit $ do
    xs             <- inputs linputs
    ys             <- inputs linputs
    nys            <- mapM circNot ys
    xsnys          <- zipWithM circMul xs nys --digit by digit x and (!y)
    nxsxory        <- zipWithM circXor xs ys -- digit by digit !(x XOR y) i.e. x and y are the same
    nxsxorys       <- mapM circNot nxsxory
    andedxors      <- chain circProd nxsxorys --list of previous XORs ANDed together
    let firstOr    = head xsnys
    let tlxsnys      = tail xsnys
    let frntandedxors  = init andedxors
    restOrs        <- zipWithM circMul tlxsnys frntandedxors
    let sofar      = firstOr : restOrs
    final          <- circSum sofar
    output final
  where
    chain f (x:xs)   = do
        rest <- helpchain f xs [x]
        return (x:rest)

    helpchain f [] _ = do 
        return []

    helpchain f (x:xs) ys = do
        z    <- f (x:ys)
        rest <- helpchain f xs (x:ys)
        return (z:rest)

