module Pattern (Model(NAT, SEQ, SEQ_END, SUB, CHAR_SEQ), Range, pattern, addChar, flatten
) where

import Debug.Trace
--import Data.Strings
import Data.List
import Data.Maybe
import Utils

data Model = NAT Int | SEQ String | SEQ_END String | SUB [Model] | CHAR_SEQ [Char] deriving (Eq)
data FlatModel = FLAT_ENV Int Int deriving (Eq)
data Range = Range Int Int deriving (Eq)

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


{-flatten :: [Model] -> Dna -> Maybe(Int, [(Int,Int)])
flatten ms dna = flatten_ Nothing 0 ms dna
--flatten ms = foldl ()

flatten_ :: Maybe(Int) -> Int -> [Model] -> Dna -> Maybe(Int,[(Int,Int)])
--flatten_ m i ms | trace ("Flatten "++show m++" "++show i ++ " " ++ show ms ) False = undefined
flatten_ m i [] _
    | isNothing m = Just (i,[])
    | otherwise = Just (i,[(mark,i-mark)])
        where mark = fromJust m
flatten_ m i (NAT n:ms) dna = flatten_ m (i+n) ms dna
flatten_ m i (SUB ss:ms) dna
    | isJust subRes && isJust tailRes = Just (justFst tailRes, justSnd subRes ++ justSnd tailRes)
    | otherwise = Nothing
    where
        subRes = flatten_ (Just i) i ss dna
        tailRes = flatten_ m (justFst subRes)  ms dna
--        (ind, rs) = flatten_ (Just i) i ss
--        (newInd, newRs) = flatten_ m ind ms
flatten_ m i (CHAR_SEQ cs:ms) dna
    | isFound = flatten_ m (i+length cs) ms dna
    | otherwise = Nothing
    where isFound = isPrefixOf cs (strDrop i dna)
flatten_ m i (SEQ_END cs:ms) dna
    | isJust match = flatten_ m (i+length cs+length (justFst match)) ms dna
    | otherwise = Nothing
    where match = splitAtTerm cs (strDrop i dna)-}



--flat :: Model -> Flat ->



instance Show Model where
  show (NAT n) = '!':show n
  show (SEQ s) = show $ '?':s
  show (SEQ_END s) = show $ "??" ++ s
  show (SUB p) = "(" ++ (show p) ++")"
  show (CHAR_SEQ s) = show s

instance Show Range where
    show (Range f t) = "("++show f++":"++show t++")"
