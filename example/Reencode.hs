module Main where

import Sound.MIDI
import Data.ByteString as B
import Data.ByteString.Lazy as L

import System.Environment

main :: IO ()
main = do
    [fp] <- getArgs
    file <- B.readFile fp
    case decodeMidi file of
        Right x -> let y = encodeMidi x in L.writeFile (fp ++ "-re") y
        Left  e -> print e
