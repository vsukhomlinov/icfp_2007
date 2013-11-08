import Data.List
import Data.Array.Repa ( Array, DIM2, DIM3, Z(..), (:.)(..))
import qualified Data.Array.Repa.Unsafe as Repa
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.DevIL as D
--import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as V


type Pixel = (Int, Int)
type Color = (Int, Int, Int)
type AlphaColor = (Color, Int)
data Direction = N|E|S|W deriving (Eq)

data Pen = Pen {
    position::Pixel,
    bucket::[Color],
    transparency::[Int],
    direction::Direction,
    mark::Pixel
}


main :: IO ()
main = runIL $ do
  x <- readImage "tmp/x.png"
--  s <- (Z :. 300 :. 200)
--  arr <- R.fromFunction s tst
  writeImage "tmp/y.png" x
--    foldl loop (splitEvery 7 rna) ((0,0), (0,0,0))

tst :: DIM2 -> DIM2
tst a = a


loop :: Pen -> String -> Pen
loop p "PIPIIIC" = addColor p (0,0,0)
loop p "PIPIIIP" = addColor p (255,0,0)
loop p "PIPIICC" = addColor p (0,255,0)
loop p "PIPIICF" = addColor p (255,255,0)
loop p "PIPIICP" = addColor p (0,0,255)
loop p "PIPIIFC" = addColor p (255,0,255)
loop p "PIPIIFF" = addColor p (0,255,255)
loop p "PIPIIPC" = addColor p (0,0,0)
loop p "PIPIIPF" = addTrans p 0
loop p "PIPIIPP" = addTrans p 255
loop p "PIIPICP" = emptyBucket p
loop p "PIIIIIP" = move p
loop p "PCCCCCP" = turnCCW p
loop p "PFFFFFP" = turnCW p
loop p "PCCIFFP" = markPos p
--loop p "PFFICCP" = makeLine p




addColor :: Pen -> Color -> Pen
addColor p c =
    Pen {position=position p,
         bucket=(c:bucket p),
         transparency=transparency p,
         direction=direction p,
         mark=mark p}

addTrans :: Pen -> Int -> Pen
addTrans p i =
    Pen {position=position p,
         bucket=bucket p,
         transparency=(i:transparency p),
         direction=direction p,
         mark=mark p}

emptyBucket :: Pen -> Pen
emptyBucket p =
    Pen {position=position p,
         bucket=[],
         transparency=transparency p,
         direction=direction p,
         mark=mark p}

move :: Pen -> Pen
move p
    | N == dir = setPosition p (x, y-1)
    | S == dir = setPosition p (x, y+1)
    | E == dir = setPosition p (x+1, y)
    | W == dir = setPosition p (x-1, y)
    where
        dir = direction p
        (x,y) = position p

turnCCW :: Pen -> Pen
turnCCW p
    | N == dir = setDirection p W
    | E == dir = setDirection p N
    | S == dir = setDirection p E
    | W == dir = setDirection p S
    where dir = direction p

turnCW :: Pen -> Pen
turnCW p
    | N == dir = setDirection p E
    | E == dir = setDirection p S
    | S == dir = setDirection p W
    | W == dir = setDirection p N
    where dir = direction p

markPos :: Pen -> Pen
markPos p =
    Pen {position=position p,
         bucket=bucket p,
         transparency=transparency p,
         direction=direction p,
         mark=position p}

--makeLine :: Pen -> Pen
{-makeLine p =
    where
        rgba = getColor p
        board = board P-}

line :: Pixel -> Pixel -> [Pixel]
line (x0,y0) (x1,y1) = zip [div a d | a<-[x, x+deltax .. x+(deltax*d)]] [div b d | b<-[y, y+deltay .. y+(deltay*d)]]
    where
        deltax = x1-x0
        deltay = y1-y0
        d = max (abs deltax) (abs deltay)
        c = if deltax*deltay <= 0 then 1 else 0
        x = x0*d + div (d-c) 2
        y = y0*d + div (d-c) 2


getColor :: Pen -> AlphaColor
getColor p
    | bucket p == [] = ((0,0,0), t)
    | otherwise = (avg summ divv (0,0,0) (bucket p), t)
    where
        t = getTrans p
        avgcol s l = div ((div s l) * t) 255
        summ (a1,b1,c1) (a2,b2,c2) = (a1+a2,b1+b2,c1+c2)
        divv (a,b,c) l = (avgcol a l, avgcol b l, avgcol c l)


getTrans :: Pen -> Int
getTrans p
    | transparency p == [] = 255
    | otherwise = avg (+) div 0 (transparency p)

avg :: (a -> a -> a) -> (a -> Int -> a) -> a -> [a] -> a
avg summ divv  initial xs = divv r l
    where (r, l) = foldl' (\(s,l) a -> (summ s a,l+1)) (initial, 0) xs

setPosition :: Pen -> Pixel -> Pen
setPosition pen (-1, y) = setPosition pen (599,y)
setPosition pen (600, y) = setPosition pen (0,y)
setPosition pen (x, -1) = setPosition pen (x,599)
setPosition pen (x, 600) = setPosition pen (x,0)
setPosition p pos =
    Pen {position=pos,
         bucket=bucket p,
         transparency=transparency p,
         direction=direction p,
         mark=mark p}

setDirection :: Pen -> Direction -> Pen
setDirection p dir =
    Pen {position=position p,
         bucket=bucket p,
         transparency=transparency p,
         direction=dir,
         mark=mark p}


splitEvery _ [] = []
splitEvery n xs = chunk : splitEvery n remaining
    where (chunk, remaining) = splitAt n xs



instance Show Pen where
    show p = show ("["++show (position p)++"], "++show (getColor p) ++ " "++show (direction p) )

instance Show Direction where
    show W = ">"
    show E = "<"
    show N = "^"
    show S = "U"

