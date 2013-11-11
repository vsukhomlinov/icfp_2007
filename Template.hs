module Template(template, Model(T_CHAR_SEQ, REF, LEN)) where

import Debug.Trace
import Utils

data Model = REF Int Int | LEN Int | T_CHAR_SEQ [Char]

template :: Dna -> ([Model], (Dna, Rna))
--template s | trace (" >template\t "++s) False = undefined
template [] = ([],([],[]))
template ('C':xs) = (addChar 'I' t, r) where (t,r) = template xs
template ('F':xs) = (addChar 'C' t, r) where (t,r) = template xs
template ('P':xs) = (addChar 'F' t, r) where (t,r) = template xs
template ('I':'C':xs) = (addChar 'P' t, r) where (t,r) = template xs
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

addChar :: Char -> [Model] -> [Model]
addChar c (T_CHAR_SEQ ch:ms) = T_CHAR_SEQ (c:ch):ms
addChar c m = T_CHAR_SEQ (c:[]):m



instance Show Model where
  show (T_CHAR_SEQ a) = show a
  show (REF n l) = show n ++ "@" ++ show l
  show (LEN n) = '|': show n ++ "|"