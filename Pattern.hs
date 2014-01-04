module Pattern (Model(NAT, SEQ, SEQ_END, SUB, CHAR_SEQ), FlatModel(FLAT_ENV), pattern, flatten, addChar
) where

import Debug.Trace
import Utils

data Model = NAT Int | SEQ String | SEQ_END String | SUB [Model] | CHAR_SEQ [Char] deriving (Eq)
data FlatModel = FLAT_ENV Int Int deriving (Eq)

pattern :: Dna -> ([Model], (Dna, Rna))
pattern [] = ([],([],[]))
pattern ('C':xs) = (addChar 'I' p, r) where (p, r) = pattern xs
pattern ('F':xs) = (addChar 'C' p, r) where (p, r) = pattern xs
pattern ('P':xs) = (addChar 'F' p, r) where (p, r) = pattern xs
pattern ('I':'C':xs) = (addChar 'P' p, r) where (p, r) = pattern xs
pattern ('I':'P':xs) = (NAT i:p, r)
    where (i,n) = nat xs
          (p,r) = pattern n
pattern ('I':'F':_:xs) = (addSeq s p, r)
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

addSeq :: String -> [Model] -> [Model]
addSeq s (CHAR_SEQ cs:ms)
    | s == cs = SEQ_END s:ms
    | otherwise = SEQ s:CHAR_SEQ cs:ms

addChar :: Char -> [Model] -> [Model]
addChar c (CHAR_SEQ ch:ms) = CHAR_SEQ (c:ch):ms
addChar c m = CHAR_SEQ (c:[]):m


--

flatten :: [Model] -> [FlatModel]
flatten [] = []
flatten ms = []



instance Show Model where
  show (NAT n) = '!':show n
  show (SEQ s) = show $ '?':s
  show (SEQ_END s) = show $ "??" ++ s
  show (SUB p) = "(" ++ (show p) ++")"
  show (CHAR_SEQ s) = show s

instance Show FlatModel where
    show (FLAT_ENV i l) = "{"++(show i)++"}"
