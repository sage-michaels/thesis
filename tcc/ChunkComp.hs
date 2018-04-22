{-# LANGUAGE TupleSections #-}

module Examples.ChunkComp where

import Circuit
import Circuit.Builder
import Control.Monad

export :: [(String, [IO (String, Acirc)])] 
export = [("ccomp8_4", [return ("ccomp8_4", ccomp 8)])]
--alter input size in the above line

ccomp :: Int -> Circuit ArithGate -- Circuit (Circuit ArithGate, CircuitArithGate)
ccomp linputs = buildCircuit $ do
    xs             <- inputs linputs
    ys             <- inputs linputs
    x              <- makecircuit linputs xs ys
    output x

--inchunk:: [Ref] -> [Ref] -> [Ref]
inchunk :: (Gate g, Monad m) => [Ref] -> [Ref] -> BuilderT g m [Ref]
inchunk as bs = do
    nbs            <- mapM circNot bs
    asnbs          <- zipWithM circMul as nbs --digit by digit a and (!b)
    nasxorb        <- zipWithM circXor as bs 
    nasxorbs       <- mapM circNot nasxorb -- digit by digit !(x XOR y) i.e. x and y are the same
    andednxors      <- chain circProd nasxorbs --list of previous XORs ANDed together ex: (x1 NXOR y1), (x1 NXOR y1)AND(x2 NXOR y2),...  
    let firstOr    = head asnbs
    let tlxsnys      = tail asnbs
    let frntandedxors  = init andednxors
    restOrs        <- zipWithM circMul tlxsnys frntandedxors
    let sofar      = firstOr : restOrs
    g              <- circSum sofar
    let eq = last andednxors
    -- return [final, last andednxors]
    --let g =          inchunk xs ys
    return [g, eq]


--chunk:: Int -> [Ref] -> [Ref] -> [Ref]
chunk:: (Gate g, Monad m) => Int -> [Ref] -> [Ref] -> BuilderT g m [Ref]
chunk n xs ys =
      if n > 8 --alter chunk by changing what is currently 8
        then do
            let h = quot n 2
            let m = n - h
            [g1, eq1] <- chunk h (take h xs) (take h ys)
            [g2, eq2] <- chunk m (drop h xs) (drop h ys)
            eq        <- circMul eq1 eq2
            g         <- circOr g1 =<< circMul eq1 g2
            return [ g, eq]
        else do
            inchunk xs ys

--makecircuit:: Int -> [Ref] -> [Ref] -> m Ref
makecircuit:: (Gate g, Monad m) => Int -> [Ref] -> [Ref] -> BuilderT g m Ref
makecircuit n xs ys = do
    c <- chunk n xs ys
    return $ head c

chain f (x:xs)   = do
        rest <- helpchain f xs [x]
        junk <- circNot x
        return (x:rest)

helpchain f [] _ = do return [] 

helpchain f (x:xs) ys = do
        z    <- f (x:ys)
        rest <- helpchain f xs (x:ys)
        return (z:rest)
