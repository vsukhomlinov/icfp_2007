import Pattern
import Template
import Data.Maybe
import Utils
import Debug.Trace
import System.Environment

main = do
--    (pref:_) <- getArgs
    writeFile "tmp/rna.txt" ""
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
    let (ps, (pdna,prna)) = pattern dna
        (ts, (tdna,trna)) = template pdna
        justM = match ps (tdna,[])
        (mdna, env) = justSnd justM
        r = replace ts env
        (newDna, rna) = if (isJust justM)
                            then (r ++ mdna, prna++trna)
                            else (tdna, prna++trna)
--        (newDna, rna) = loop dna
    putStrLn $ "match: " ++ (show (isJust justM))
    putStrLn $ "DNA after template: " ++ (show $ length tdna)
    appendFile "tmp/rna.txt" rna
    putStrLn $ show ps ++ "    " ++ show ts
--    putStrLn $ "ENV: " ++ (show env)
--    putStrLn $ "Old DNA: " ++ (show $ length mdna)
--    putStrLn $ "Attached DNA: " ++ (show $ length r)
    loopOne newDna



loop :: Dna -> (Dna, Rna)
loop [] = error "Finish"
loop xs
    | isJust justM = (r ++ mdna, rna++trna)
    | otherwise = (mdna, rna++trna)
    where (ps, (dna,rna)) = pattern xs
          (ts, (tdna,trna)) = template dna
          justM = match ps (tdna,[])
          (mdna, env) = justSnd justM
          r = replace ts env

match :: [Pattern.Model] -> (String, [Env]) -> Maybe (String, (String, [Env]))
match [] (xs,es) = Just([], (xs, es))
--match a (b,c) | trace ("Match "++show a++" "++show (length c) ) False = undefined
match ((CHAR c):ps) ((x:xs), es)
    | x == c && isJust res = Just (x:(justFst res), justSnd res)
    | otherwise = Nothing
    where res = match ps (xs,es)
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
match ((SUB s):ps) (xs, es)
    | isJust res && isJust next = Just (matched++(justFst next), justSnd next)
    | otherwise = Nothing
    where
        res = match s (xs,es)
        matched = (justFst res)
        envs = justSnd res
        next = match ps (fst $ envs, (snd $ envs) ++ [ENV $ matched])

replace :: [Template.Model] -> [Env] -> String
--replace a b | trace ("Replace "++show a++" "++show b ) False = undefined
replace [] _ = []
replace ((T_CHAR c):ts) es = c:(replace ts es)
replace ((REF n l):ts) es = (protect e l) ++ (replace ts es) where (ENV e) = es !! n
replace ((LEN n):ts) es = (asnat $ length e) ++ (replace ts es) where (ENV e) = es !! n

--matchOne ps xs = error ("Error "++ (show xs))
--matchOne xs (SUB s) = if (isJust t) then Just (Nothing, fromJust t) else Nothing where t = dropWhileFound s xs

data Search = FOUND String | NOT_FOUND

data Match = NEXT String | RETURN String | RESULT String String
data Env = ENV String

instance Show Match where
  show (NEXT s) = "> "++s
  show (RETURN s) = "RETURN " ++ s
  show (RESULT a b) = show a

instance Show Search where
  show (FOUND s) = show s
  show NOT_FOUND = "Not found"

instance Show Env where
    show (ENV s) = show $ length s



