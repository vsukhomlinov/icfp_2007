module Template(template, Model(T_CHAR, REF, LEN)) where

import Debug.Trace
import Utils

data Model = T_CHAR Char | REF Int Int | LEN Int

template :: Dna -> ([Model], (Dna, Rna))
--template s | trace (" >template\t "++s) False = undefined
template [] = ([],([],[]))
template ('C':xs) = (T_CHAR 'I':t, r) where (t,r) = template xs
template ('F':xs) = (T_CHAR 'C':t, r) where (t,r) = template xs
template ('P':xs) = (T_CHAR 'F':t, r) where (t,r) = template xs
template ('I':'C':xs) = (T_CHAR 'P':t, r) where (t,r) = template xs
template ('I':'F':xs) = (m:ts,(d,r))
    where
        (m, dna) = ref xs
        (ts, (d,r)) = template dna
template ('I':'P':xs) = template ('I':'F':xs)
template ('I':'I':'C':xs) =  ([],(xs,""))
template ('I':'I':'F':xs) =  ([],(xs,""))
template ('I':'I':'P':xs) =  (LEN n:t,r)
    where (n,nr) = nat xs
          (t, r) = template nr
template ('I':'I':'I':xs) = (ts, (d,rna++r))
    where
        (rna,dna) = splitAt 7 xs
        (ts, (d,r)) = template dna

ref :: String -> (Model, Dna)
ref xs = (REF n l, nr)
     where (l,lr) = nat xs
           (n,nr) = nat lr


instance Show Model where
  show (T_CHAR a) = show a
  show (REF n l) = show n ++ "@" ++ show l
  show (LEN n) = '|': show n ++ "|"