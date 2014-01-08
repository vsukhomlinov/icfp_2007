import Pattern
import Template
import Data.Maybe
import Data.List
import Utils
import Debug.Trace
import System.Environment

main = do
--    (pref:_) <- getArgs
    writeFile "tmp/rna.txt" ""
    writeFile "tmp/match.log" ""
    text <- readFile "resources/endo.dna"
    loop (seq_debug ++ text)

--IPIFFCPICFPPICIICCIICIPPPFIIC

--seq_1::String
seq_debug="IIPIFFCPICICIICPIICIPPPICIIC"
seq_hlp="IIPIFFCPICFPPICIICCIICIPPPFIIC"



loop :: Dna -> IO ()
loop [] = error "Finish"
loop dna = do
    let (ps, (pdna,prna)) = pattern dna
        (ts, (tdna,trna)) = template pdna
        rna = prna++trna
        flat = mergeModels ps ts tdna

        newDna = foldl (\acc m ->  acc++(getChunk tdna m)) [] flat
    appendFile "tmp/rna.txt" rna
--    appendFile "tmp/match.log" ((show ps) ++ (show ts)++"\n")
    appendFile "tmp/match.log" (show flat ++"\n")

    putStr "."
    loop newDna

getChunk :: Dna -> FlatModel -> Dna
getChunk _ (RANGE _ 0) = []
getChunk dna (RANGE i l) = take l (drop i dna)
getChunk dna (CHARS s) = s

mergeModels :: [Pattern.Model] -> [Template.Model] -> Dna -> [FlatModel]
mergeModels ps ts dna
    | isNothing flatPattern = [RANGE 0 $ length dna]
    | otherwise  = (map (mergeSort dna ranges) (ts)) ++ [RANGE dnaIndex (length dna - dnaIndex)]
    where
        flatPattern = flatten ps dna
        dnaIndex = justFst flatPattern

        ranges = justSnd flatPattern

mergeSort :: Dna -> [(Int,Int)] -> Template.Model -> FlatModel
mergeSort _ rgs (REF n 0) = RANGE i l
    where (i,l) = if (length rgs > n) then rgs !! n else (0,0)
mergeSort dna rgs (REF n p) = CHARS $ protect (take l (drop i dna)) p
    where (i,l) = if (length rgs > n) then rgs !! n else (0,0)
mergeSort _ _ (T_CHAR_SEQ s) = CHARS s
mergeSort _ rgs (LEN n) = CHARS $ asnat l
    where (_,l) = if (length rgs > n) then rgs !! n else (0,0)


data FlatModel = CHARS String | RANGE Int Int

instance Show FlatModel where
    show (CHARS s) = show s
    show (RANGE i l) = "("++show i++" "++show l++")"



