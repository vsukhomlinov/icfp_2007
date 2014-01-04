module Utils (consts, nat, dropWhileFound, splitAtTerm, splitAfterTerm, protect, asnat, justFst, justSnd, Dna, Rna) where

import Data.List
import Data.Maybe
import Data.Strings

consts :: [Char] -> ([Char],[Char])
consts ('C':xs) = ('I':c, s) where (c,s) = consts xs
consts ('F':xs) = ('C':c, s) where (c,s) = consts xs
consts ('P':xs) = ('F':c, s) where (c,s) = consts xs
consts ('I':'C':xs) = ('P':c, s) where (c,s) = consts xs
consts xs = ([],xs)

nat :: [Char] -> (Int, [Char])
nat [] = error "Finish"
nat ('P':xs) = (0, xs)
nat ('I':xs) = (2 * i, s) where (i,s) = nat xs
nat ('F':xs) = (2 * i, s) where (i,s) = nat xs
nat ('C':xs) = (1+2 * i, s) where (i,s) = nat xs

dropWhileFound :: String -> String -> Maybe String
dropWhileFound [] s = Just s
dropWhileFound _ [] = Nothing
dropWhileFound term source
    | isPrefixOf term source = Just source
    | otherwise  = dropWhileFound term rest
    where (_:rest) = source

splitAtTerm :: String -> String -> Maybe (String, String)
splitAtTerm [] s = Just ([], s)
splitAtTerm _ [] = Nothing
--splitAtTerm term source
--    | isPrefixOf term source = Just ([], source)
--    | otherwise  = ret
--    where
--        (x:rest) = source
--        res = splitAtTerm term rest
--        (h,t) = fromJust res
--        ret = if (isJust res) then Just (x:h, t) else Nothing
splitAtTerm term source
    | t == [] = Nothing
    | otherwise = Just (h, t)
    where (h,t) = strBreak term source

splitAfterTerm term source
    | t == []  && source == h = Nothing
    | otherwise = Just (strAppend term h, t)
    where (h,t) = strSplit term source



protect :: String -> Int -> String
protect xs 0 = xs
protect xs n = protect (quote xs) (n-1)

quote :: String -> String
quote [] = []
quote ('I':xs) = 'C':(quote xs)
quote ('C':xs) = 'F':(quote xs)
quote ('F':xs) = 'P':(quote xs)
quote ('P':xs) = 'I':'C':(quote xs)

asnat :: Int -> String
asnat 0 = "P"
asnat n
    | even n = 'I':asnat (div n 2)
    | otherwise = 'C':asnat ( div n 2)


justFst :: (Maybe (a,b)) -> a
justFst a
    | isJust a = fst $ fromJust a
    | otherwise = error "Value is 'Nothing'"

justSnd :: (Maybe (a,b)) -> b
justSnd a
    | isJust a = snd $ fromJust a
    | otherwise = error "Value is 'Nothing'"

type Dna = String
type Rna = String
