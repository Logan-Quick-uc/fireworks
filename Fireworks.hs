module Fireworks where
import Data.Char
import System.Random
import Control.Concurrent

stringOverlap :: String -> String -> String
stringOverlap "" bs = bs
stringOverlap as "" = as
stringOverlap (a:as) (b:bs)
    | isSpace a = b : stringOverlap as bs
    | otherwise = a : stringOverlap as bs

stringsOverlap :: [String] -> [String] -> [String]
stringsOverlap [] bss = bss
stringsOverlap ass [] = ass
stringsOverlap (as:ass) (bs:bss) = stringOverlap as bs : stringsOverlap ass bss  

stringOffset :: Int -> String -> String
stringOffset 0 s = s
stringOffset i s = ' ' : stringOffset (i - 1) s

stringsOffset :: (Int, Int) -> [String] -> [String]
stringsOffset _      []     = []
stringsOffset (0, j) (s:ss) = stringOffset j s : stringsOffset (0, j) ss
stringsOffset (i, j) ss     = "": stringsOffset (i-1,j) ss

fireworkFactory :: Int -> Int -> Int -> IO [[String]]
fireworkFactory i j h 
    | i == h = do
        firework <- readFile "firework.txt"

        pure $ (stringsOffset (i, j) $ lines firework):[""]:[]
    | otherwise = do
        let firework = stringsOffset (i, j+3) ["o", "|"]
        fireworks <- fireworkFactory (i-1) j h
        pure $ firework:fireworks


framesOverlap :: [[String]] -> [[String]] -> IO [[String]]
framesOverlap bg [] = pure bg
framesOverlap (bg:bgs) (fw:fws) = do
    let frame = stringsOverlap fw bg
    frames <- framesOverlap bgs fws
    pure $ frame : frames

fireworkFrames :: StdGen -> IO ([[String]], StdGen)
fireworkFrames g0 = do
    let (j, g1) = uniformR (7 :: Int, 70) g0
    let (h, g2) = uniformR (3 :: Int, 14) g1
    fireworks <- fireworkFactory 17 j h
    pure $ (fireworks, g2)


animateFrame :: [[String]] -> IO [[String]]
animateFrame (ss:sss) = do
    putStr ("\x1b[2J")
    mapM_ putStrLn ss
    threadDelay 100000
    pure sss

fireworkAnimation :: StdGen -> [[String]] -> IO ()
fireworkAnimation g0 bg = do
    (fws, g1) <- fireworkFrames g0
    frames <- framesOverlap bg fws
    frames1 <- animateFrame frames
    frames2 <- animateFrame frames1
    frames3 <- animateFrame frames2
    fireworkAnimation g1 frames3


main :: IO ()
main = do
    start <- initStdGen
    nightFile <- readFile "night.txt"
    let bg = lines nightFile
    let bgFrames = [bg | x <- [1..]]
    fireworkAnimation start bgFrames