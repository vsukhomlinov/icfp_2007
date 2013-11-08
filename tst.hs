import Data.Array.Repa.IO.DevIL(Image(..))
import qualified Data.Array.Repa.IO.DevIL as D
import Data.Array.Repa (Z(..), (:.)(..), fromUnboxed)
import qualified Data.Array.Repa as R
import Data.Vector.Unboxed as V
import Data.Array.Repa.Repr.ForeignPtr (F, fromForeignPtr)
import Foreign.ForeignPtr.Safe as FS
import System.Directory

import Data.Word (Word8)


main :: IO ()
main = do
    let file = "tmp/tst1.png"
    let size = 100
--    removeFile file
    ptr <- ((mallocForeignPtrArray0 10000) :: IO (ForeignPtr Word8))
    let arr = getArr 100 ptr
    D.runIL $ D.writeImage file (RGBA arr)
--    putStrLn $ show arr

--let arr =


{-
getImage :: Int -> Image
getImage s = RGBA $ getArr s
-}

getArr :: Int -> ForeignPtr Word8 -> (R.Array F R.DIM3 Word8)
getArr s ptr = fromForeignPtr (Z :. s :. s :. 1) ptr


{-getArr1 :: Int -> R.Array R.U R.DIM3 Word8
getArr1 s = R.computeP $ (R.fromListUnboxed (Z :. s :. s :. (3::Int)) ps :: R.Array R.U R.DIM3 Word8)
    where ps = [0 :: Word8 | x <- [1..s*s]]-}

{-getArr2 :: Int -> R.Array R.U R.DIM3 Word8
getArr2 s =
    let arr = R.fromListUnboxed (Z :. 10 :. 10 :. (3 :: Int)) ps :: R.Array R.U R.DIM3 Word8
        ps = [1..10000]
--        arr2 = R.computeP arr
    in RGB arr-}
