module Top (main) where

import Data.Word (Word8)
import qualified Data.ByteString as BS

main :: IO ()
main = do
  putStrLn "*fb*"
  let filename = "data.out"
  BS.writeFile filename $ BS.pack pixs

pixs :: [Word8]
pixs =
  [ pix
  | y <- [0.. h - 1]
  , x <- [0.. w - 1]
  , let (r,g,b) = col x y
  , pix <- [b,g,r,0]
  ]
  where
    w = 1920
    h = 1080

    col :: Int -> Int -> (Word8,Word8,Word8)
    col x y = (round r, round g, 0)
      where
         r :: Float = fromIntegral x / (fromIntegral w / 256)
         g :: Float = fromIntegral y / (fromIntegral h / 256)
