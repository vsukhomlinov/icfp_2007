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
    loopOne (seq_debug ++ text)
--    let (dna, rna) = loop text
--    appendFile "tmp/rna.txt" rna
--    putStr "."
--    putStrLn $ show r where (d,r) = loop "IICIIC"

--IPIFFCPICFPPICIICCIICIPPPFIIC

--seq_1::String
seq_debug="IIPIFFCPICICIICPIICIPPPICIIC"
seq_hlp="IIPIFFCPICFPPICIICCIICIPPPFIIC"



loopOne :: Dna -> IO ()
loopOne [] = error "Finish"
loopOne dna = do
    loopDna <- loop dna
    let (ps, (pdna,prna)) = pattern dna
        (ts, (tdna,trna)) = template pdna
        justM = match ps (tdna,[])
        (mdna, env) = justSnd justM
        r = replace ts env
        (newDna, rna) = if (isJust justM)
                            then (r ++ mdna, prna++trna)
                            else (tdna, prna++trna)

--    appendFile "tmp/rna.txt" rna
--    appendFile "tmp/match.log" ((show ps) ++ (show ts)++"\n")
--    appendFile "tmp/match.log" (show (flatten ps tdna)++"\n")
--    putStrLn $ show ps ++ "    " ++ show ts
--    putStrLn $ show (flatten ps tdna)
    putStr "."

    putStrLn $ show (length newDna == (length loopDna))
--    loop dna
    loopOne loopDna

loop :: Dna -> IO (Dna)
loop [] = error "Finish"
loop dna = do
    let (ps, (pdna,prna)) = pattern dna
        (ts, (tdna,trna)) = template pdna
        rna = prna++trna
        flat = mergeModels ps ts tdna

        newDna = foldl (\acc m ->  acc++(getChunk tdna m)) [] flat

--        newDna = if(isJust flat)
--                    then ""
--                        loop $ prepareDna refs tpl tdna
--                    else ""
    appendFile "tmp/rna.txt" rna
--    appendFile "tmp/match.log" ((show ps) ++ (show ts)++"\n")
--    appendFile "tmp/match.log" (show flat ++"\n")

--    putStrLn (show flat)
--    loop newDna
    return newDna

getChunk :: Dna -> FlatModel -> Dna
getChunk _ (RANGE _ 0) = []
getChunk dna (RANGE i l) = take l (drop i dna)
getChunk dna (CHARS s) = s



match :: [Pattern.Model] -> (String, [Env]) -> Maybe (String, (String, [Env]))
match [] (xs,es) = Just([], (xs, es))
--match a (b,c) | trace ("Match "++show a++" "++show (length c) ) False = undefined
match ((CHAR_SEQ s):ps) (xs, es)
    | isJust rest && isJust res = Just (s ++ (justFst res), justSnd res)
    | otherwise = Nothing
    where
        rest = stripPrefix s xs
        res = match ps (fromJust rest,es)
match ((NAT n):ps) (xs,es)
    | length xs >= n && isJust res = Just (h ++ (justFst res), justSnd res)
    | otherwise = Nothing
    where
        (h,t) = splitAt n xs
        res = match ps (t,es)
match ((SEQ s):ps) (xs,es)
    | isJust split && isJust res = Just ((justFst split) ++ (justFst res), justSnd res)
    | otherwise = Nothing
    where
        split = splitAtTerm s xs
        res = match ps (justSnd $ split, es)

match ((SEQ_END s):ps) (xs,es)
    | isJust split && isJust res = Just ((justFst split) ++ (justFst res), justSnd res)
    | otherwise = Nothing
    where
        split = splitAfterTerm s xs
        res = match ps (justSnd $ split, es)

match ((SUB s):ps) (xs, es)
    | isJust res && isJust next = Just (matched++(justFst next), justSnd next)
    | otherwise = Nothing
    where
        res = match s (xs,es)
        matched = (justFst res)
        envs = justSnd res
        next = match ps (fst $ envs, (snd $ envs) ++ [ENV $ matched])

mergeModels :: [Pattern.Model] -> [Template.Model] -> Dna -> [FlatModel]
mergeModels ps ts dna
    | isNothing flatPattern = [RANGE 0 $ length dna]
    | otherwise  = map (mergeSort dna ranges) (ts++[REF (length ranges-1) 0])
    where
        flatPattern = flatten ps dna
        dnaIndex = justFst flatPattern
        ranges = justSnd flatPattern ++ [(dnaIndex, length dna - dnaIndex)]


mergeSort :: Dna -> [(Int,Int)] -> Template.Model -> FlatModel
mergeSort _ rgs (REF n 0) = RANGE i l
    where (i,l) = if (length rgs > n) then rgs !! n else (0,0)
mergeSort dna rgs (REF n p) = CHARS $ protect (take l (drop i dna)) p
    where (i,l) = if (length rgs > n) then rgs !! n else (0,0)
mergeSort _ _ (T_CHAR_SEQ s) = CHARS s
mergeSort _ rgs (LEN n) = CHARS $ asnat l
    where (_,l) = if (length rgs > n) then rgs !! n else (0,0)




replace :: [Template.Model] -> [Env] -> String
--replace a b | trace ("Replace "++show a++" "++show b ) False = undefined
replace [] _ = []
replace ((T_CHAR_SEQ c):ts) es = c ++ (replace ts es)
replace ((REF n l):ts) es = (protect e l) ++ (replace ts es)
    where (ENV e) =  if (length es > n) then es !! n else ENV ""
replace ((LEN n):ts) es = (asnat $ length e) ++ (replace ts es)
    where (ENV e) = if (length es > n) then es !! n else ENV ""

--matchOne ps xs = error ("Error "++ (show xs))
--matchOne xs (SUB s) = if (isJust t) then Just (Nothing, fromJust t) else Nothing where t = dropWhileFound s xs

data FlatModel = CHARS String | RANGE Int Int

data Env = ENV String

instance Show Env where
    show (ENV s) = show $ length s
instance Show FlatModel where
    show (CHARS s) = show s
    show (RANGE i l) = "("++show i++" "++show l++")"



