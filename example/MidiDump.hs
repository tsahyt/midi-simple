module Main where

import Sound.MIDI
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as B
import Pipes.Attoparsec
import System.IO
import System.Environment

main :: IO ()
main = do
    [fp] <- getArgs
    fd   <- openFile fp ReadMode

    runEffect $
        void (parsed midiParser (B.fromHandle fd)) >-> P.map show >-> P.stdoutLn

    return ()
