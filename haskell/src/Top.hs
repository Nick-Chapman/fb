module Top (main) where

import Control.Monad (forever)
import Data.Set (Set)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.Random

main :: IO ()
main = do
  let pureGen = mkStdGen 137
  let bools = randoms pureGen
  let states = iterate step (state0 bools)
  forever $ sequence_
    [ do
        putStrLn $ "tick" ++ show i
        writePic (makePic (genFromState s))
    | (i,s) <- zip [1::Int ..] states
    ]

type Pic = [Word8]

writePic :: Pic -> IO ()
writePic pic = BS.writeFile "/dev/fb0" $ BS.pack pic

width,height :: Int
width = 1920
height = 1080

makePic :: Gen -> Pic
makePic pic =
  [ pix
  | y <- [0.. height - 1]
  , x <- [0.. width - 1]
  , let (r,g,b) = pic x y
  , pix <- [b,g,r,0]
  ]

data State = State (Set Pos)

type Pos = (Int,Int)

genFromState :: State -> Gen
genFromState (State set) x y = do
  if (x `div` 10, y `div` 10) `Set.member` set then white else black
  where
    white = (255,255,255)
    black = (0,0,0)


state0 :: [Bool] -> State
state0 randoms = do
  State $ Set.fromList $
    [ p | (r,p) <-
          zip randoms [ (x,y)
                      | y <- [0.. height `div` 10 - 1]
                      , x <- [0.. width `div` 10 - 1]
                      ]
         , r ]

{-
_state0 :: State
_state0 = State gun
  where
    gun = Set.fromList $ map (addPos (30,50)) gunOffsets

gunOffsets :: [Pos]
gunOffsets = [(0,0),(2,0),(1,1),(2,1),(1,2)]

addPos :: Pos -> Pos -> Pos
addPos (x1,y1) (x2,y2) = (x1+x2,y1+y2)
-}

step :: State -> State
step (State set) = do
  let m = Map.fromListWith (+)
        [ (q,1::Int) | p <- Set.toList set, q <- neighbor p ]
  let has3Neighbors = [ p | (p,3) <- Map.toList m ]
  let staysAlive = [ p | p <- Set.toList set , Map.lookup p m == Just 2 ]
  State $ Set.fromList (staysAlive ++ has3Neighbors)

neighbor :: Pos -> [Pos]
neighbor (x,y) =
  [(x+1,y+1), (x,y+1), (x-1,y+1)
  ,(x+1,y  ),          (x-1,y  )
  ,(x+1,y-1), (x,y-1), (x-1,y-1)
  ]

type Gen = (Int -> Int -> (Word8,Word8,Word8))

{-
rgGradient :: Gen
rgGradient x y = (round r, round g, 0)
  where
    r :: Float = fromIntegral x / (fromIntegral width / 256)
    g :: Float = fromIntegral y / (fromIntegral height / 256)

blueScreen :: Gen
blueScreen _ _ = (0,0,255)
-}
