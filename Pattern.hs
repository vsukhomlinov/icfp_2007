module Pattern (Model(CHAR, NAT, SEQ, SUB), pattern
) where

import Debug.Trace
import Utils

data Model = CHAR Char | NAT Int | SEQ String | SUB [Model]

pattern :: Dna -> ([Model], (Dna, Rna))
--pattern s | trace (" >pattern "++s) False = undefined
pattern [] = ([],([],[]))
pattern ('C':xs) = (CHAR 'I':p, r) where (p, r) = pattern xs
pattern ('F':xs) = (CHAR 'C':p, r) where (p, r) = pattern xs
pattern ('P':xs) = (CHAR 'F':p, r) where (p, r) = pattern xs
pattern ('I':'C':xs) = (CHAR 'P':p, r) where (p, r) = pattern xs
pattern ('I':'P':xs) = (NAT i:p, r)
    where (i,n) = nat xs
          (p,r) = pattern n
pattern ('I':'F':_:xs) = (SEQ s:p, r)
    where (s, n) = consts xs
          (p,r) = pattern xs
pattern ('I':'I':'I':xs) = (ps, (d, rna++r))
    where
        (rna, dna) = splitAt 7 xs
        (ps, (d,r)) = pattern dna
pattern ('I':'I':'P':xs) = (SUB p :np, (rd, r++rr))
    where (p,(d,r)) = pattern xs
          (np, (rd,rr)) = pattern d
pattern ('I':'I':'C':xs) = ([],(xs,[]))
pattern ('I':'I':'F':xs) = ([],(xs,[]))
pattern (x:xs) = error "Finish: not matching symbol"

instance Show Model where
  show (CHAR a) = show a
  show (NAT n) = '!':show n
  show (SEQ s) = show $ '?':s
  show (SUB p) = "(" ++ (show p) ++")"