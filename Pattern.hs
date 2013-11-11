module Pattern (Model(NAT, SEQ, SEQ_END, SUB, CHAR_SEQ), pattern
) where

import Debug.Trace
import Utils

data Model = NAT Int | SEQ String | SEQ_END String | SUB [Model] | CHAR_SEQ [Char]

pattern :: Dna -> ([Model], (Dna, Rna))
--pattern s | trace (" >pattern "++s) False = undefined
pattern [] = ([],([],[]))
pattern ('C':xs) = (addChar 'I' p, r) where (p, r) = pattern xs
pattern ('F':xs) = (addChar 'C' p, r) where (p, r) = pattern xs
pattern ('P':xs) = (addChar 'F' p, r) where (p, r) = pattern xs
pattern ('I':'C':xs) = (addChar 'P' p, r) where (p, r) = pattern xs
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


addChar :: Char -> [Model] -> [Model]
--addChar c (CHAR_SEQ ch:SEQ s:ms)
--    | s == newSeq = SEQ_END s:ms
--    | otherwise = CHAR_SEQ newSeq:SEQ s:ms
--        where newSeq = ch ++ (c:[])
addChar c (CHAR_SEQ ch:ms) = CHAR_SEQ (c:ch):ms
addChar c m = CHAR_SEQ (c:[]):m

instance Show Model where
  show (NAT n) = '!':show n
  show (SEQ s) = show $ '?':s
  show (SEQ_END s) = show $ "??" ++ s
  show (SUB p) = "(" ++ (show p) ++")"
  show (CHAR_SEQ s) = show s