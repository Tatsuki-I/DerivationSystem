module Nat where

import Control.Monad

data Judgement = Plus  Int Int Int
               | Times Int Int Int
               deriving ( Eq
                        , Read )

instance Show Judgement where
    show (Plus  n1 n2 n3) = show n1 ++ " plus "
                                    ++ show n2 
                                    ++ " is "
                                    ++ show n3

    show (Times n1 n2 n3) = show n1 ++ " times "
                                    ++ show n2
                                    ++ " is "
                                    ++ show n3

data Rule = PZERO
          | PSUCC Judgement
          | TZERO
          | TSUCC Judgement Judgement

instance Show Rule where
    show PZERO         = "By P-ZERO."
    show (PSUCC j)     = "By P-SUCC, with a judgement " ++ show j
    show TZERO         = "By T-ZERO"
    show (TSUCC j1 j2) = "By T-SUCC, with judgements " ++ show j1 ++ " and " ++ show j2

derivation                    :: Judgement -> IO (Judgement, [(Judgement, Rule)])
derivation j@(Plus  n1 n2 n3) =  do print j
                                    putStrLn (replicate ((length . show) j) '-')
                                    pDerivation j

derivation j@(Times n1 n2 n3) =  do print j
                                    putStrLn "----------"
                                    tDerivation j

pDerivation                   :: Judgement -> IO (Judgement, [(Judgement, Rule)])
pDerivation j@(Plus n1 n2 n3) =  do forM_ (res) $ \i -> do
                                          print i
                                    return (j, res)
                                    where pz = pZero n2
                                          res =pSuccEval n1 pz

pSuccEval :: Int -> (Judgement, Rule) -> [(Judgement, Rule)]
pSuccEval    a jr@(j@(Plus n1 _ _), _)
             | a < n1   = []
             | otherwise = jr : pSuccEval a (pSucc j)

tDerivation                    :: Judgement -> IO (Judgement, [(Judgement, Rule)])
tDerivation j@(Times n1 n2 n3) =  do putStrLn "By T-Zero"
                                     res <- tSuccEval n1 tz
                                     return (j, [(j, TZERO)])
                                     where tz = tZero n2

tSuccEval :: Int -> Judgement -> IO Judgement
tSuccEval = undefined


pZero   :: Int -> (Judgement, Rule)
pZero n =  (Plus 0 n n, PZERO)

pSucc                 :: Judgement -> (Judgement, Rule)
pSucc j@(Plus n1 n2 n3) =  (Plus (succ n1) n2 (succ n3), PSUCC j)

tZero   :: Int -> Judgement
tZero n =  Times 0 n 0

tSucc   :: Judgement -> Judgement -> Maybe Judgement
tSucc (Times n1 n2 n3) (Plus n4 n5 n6) = if n2 == n4 && 
                                            n3 == n5
                                            then Just (Times (succ n1) n2 n6)
                                            else Nothing

