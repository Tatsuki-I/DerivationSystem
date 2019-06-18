module Nat where

import Control.Monad
import Data.Maybe

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

isPlus         :: Judgement -> Bool
isPlus Plus {} =  True
isPlus _       =  False

printDerivation   :: Judgement -> IO (Judgement, [(Rule, Judgement)])
printDerivation j =  do print j
                        putStrLn (replicate ((length . show) j) '-')
                        forM_ (snd res) $ \i -> print i
                        return res
                        where res = derivation j

derivation                    :: Judgement -> (Judgement, [(Rule, Judgement)])
derivation j | isPlus j  = pDerivation j
             | otherwise = tDerivation j

pDerivation                   :: Judgement -> (Judgement, [(Rule, Judgement)])
pDerivation j@(Plus n1 n2 n3) =  (j, pSuccEval n1 (pZero n2))

pSuccEval                              :: Int -> (Rule, Judgement) -> [(Rule, Judgement)]
pSuccEval    a jr@(_, j@(Plus n1 _ _)) |  a < n1   = []
                                       |  otherwise = jr : pSuccEval a (pSucc j)

tDerivation                    :: Judgement -> (Judgement, [(Rule, Judgement)])
tDerivation j@(Times n1 n2 n3) =  (j, tSuccEval n1 (tZero n2))

tSuccEval :: Int -> (Rule, Judgement) -> [(Rule, Judgement)]
tSuccEval    a jr@(_, j@(Times n1 _ _)) | a < n1   = []
                                        | otherwise = jr : tSuccEval a (fromJust (tSucc j (pDerivation foobar)))


pZero   :: Int -> (Rule, Judgement)
pZero n =  (PZERO, Plus 0 n n)

pSucc                   :: Judgement -> (Rule, Judgement)
pSucc j@(Plus n1 n2 n3) =  (PSUCC j, Plus (succ n1) n2 (succ n3))

tZero   :: Int -> (Rule, Judgement)
tZero n =  (TZERO, Times 0 n 0)

tSucc                                  :: Judgement -> Judgement -> Maybe Judgement
tSucc (Times n1 n2 n3) (Plus n4 n5 n6) = if n2 == n4 && 
                                            n3 == n5
                                            then Just (Times (succ n1) n2 n6)
                                            else Nothing

