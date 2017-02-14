{-# LANGUAGE OverloadedStrings #-}
module Main where

import Sound.MIDI
import Sound.MIDI.Types
import Criterion.Main
import Data.ByteString (ByteString)

pchannelVoice, ppitchBend, pmonoOn, pexclusive :: ByteString
pchannelVoice = "\x80\x3c\x7f"
ppitchBend = "\xe0\x08\x00"
pmonoOn = "\xb0\x7e\x03"
pexclusive = "\xf0\x00\x08\x08\x30\x30\x30\xf7"

pmulti :: ByteString
pmulti = "\x80\x3c\x7f\xf0\x00\x08\x08\x30\x30\x30\xf7\xe0\x08\x00\xb0\x7e\x03"

schannelVoice, spitchBend, smonoOn, sexclusive :: MidiMessage
schannelVoice = ChannelVoice (NoteOff (Channel 0) (Pitch 60) (Velocity 127))
spitchBend = ChannelVoice (PitchBend (Channel 0) (to14Bit 8))
smonoOn = ChannelMode (MonoOn (Channel 0) (to7Bit 3))
sexclusive = 
    SystemExclusive (Exclusive (VendorIdLong (to7Bit 8) (to7Bit 8)) "000")

smulti = [ schannelVoice, spitchBend, smonoOn, sexclusive ]

main :: IO ()
main = defaultMain
    [ bgroup "parsing"
        [ bench "channelVoice" $ whnf decodeMidi1 pchannelVoice
        , bench "pitchBend" $ whnf decodeMidi1 ppitchBend
        , bench "monoOn" $ whnf decodeMidi1 pmonoOn
        , bench "exclusive" $ whnf decodeMidi1 pexclusive
        , bench "multi" $ whnf decodeMidi pmulti
        ]
    , bgroup "serializing"
        [ bench "channelVoice" $ whnf encodeMidi1' schannelVoice
        , bench "pitchBend" $ whnf encodeMidi1' spitchBend
        , bench "monoOn" $ whnf encodeMidi1' smonoOn
        , bench "exclusive" $ whnf encodeMidi1' sexclusive
        , bench "multi" $ whnf encodeMidi' smulti
        ]
    ]
