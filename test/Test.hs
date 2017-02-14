{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString.Lazy (toStrict)
import Data.Word
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Hspec
import Test.Hspec.Attoparsec

import Sound.MIDI
import Sound.MIDI.Types
import qualified Sound.MIDI.Serialize as S
import qualified Sound.MIDI.Parser as P

main :: IO ()
main = do
    st <- specTests
    defaultMain (testGroup "Tests" [ tests, st ])

tests :: TestTree
tests = testGroup "QuickCheck"
    [ testProperty "word14: parse . serialize == id" $
          \(w16 :: Word16) ->
              let w14  = to14Bit w16
                  w14' = A.parseOnly P.anyWord14 . toStrict . B.toLazyByteString 
                       . S.word14 $ w14
               in Right w14 == w14'
    , testProperty "parse . serialize == id" $
          \(msg :: MidiMessage) ->
              decodeMidi1 (encodeMidi1' msg) == Right msg
    , testProperty "parse . serialize == id (long)" $
          \(msg :: NonEmptyList MidiMessage) ->
              let msg' = getNonEmpty msg
               in fmap mergeEOX (decodeMidi (encodeMidi' msg')) == Right msg'
    ]

-- | The parser will separate EOXs by design, hence they need to be merged back
-- into the exclusive message during property testing.
mergeEOX :: [MidiMessage] -> [MidiMessage]
mergeEOX [] = []
mergeEOX (x@(SystemExclusive _) : SystemCommon EOX : xs) = x : mergeEOX xs
mergeEOX (x:xs) = x : mergeEOX xs

specTests :: IO TestTree
specTests = testSpec "Hspec" . sequence_ . map go $ specCases
    where go (SC n msg str) =
              describe n $ do
                  it "parses" $ decodeMidi1 str `shouldParse` msg
                  
                  it "serializes" $ encodeMidi1' msg `shouldBe` str

data SpecCase = SC String MidiMessage B.ByteString

specCases :: [SpecCase]
specCases = 
    [ -- Channel Voice tests
      SC "Note Off"
        (ChannelVoice (NoteOff (Channel 0) (Pitch 60) (Velocity 127)))
        "\x80\x3c\x7f"
    , SC "Note On"
        (ChannelVoice (NoteOn (Channel 0) (Pitch 60) (Velocity 0)))
        "\x90\x3c\x00"
    , SC "Aftertouch"
        (ChannelVoice (Aftertouch (Channel 1) (Pitch 0) (Touch 64)))
        "\xa1\x00\x40"
    , SC "Control Change"
        (ChannelVoice (ControlChange (Channel 0) (Controller 4) (to7Bit 0)))
        "\xb0\x04\x00"
    , SC "Patch Change"
        (ChannelVoice (PatchChange (Channel 4) (Patch 8)))
        "\xc4\x08"
    , SC "Channel Pressure"
        (ChannelVoice (ChannelPressure (Channel 4) (Touch 64)))
        "\xd4\x40"
    , SC "Pitch Bend"
        (ChannelVoice (PitchBend (Channel 0) (to14Bit 8)))
        "\xe0\x08\x00"
      -- Channel Mode test
    , SC "All Sound Off"
        (ChannelMode (AllSoundOff (Channel 0)))
        "\xb0\x78\x00"
    , SC "Reset Controllers"
        (ChannelMode (ResetAllControllers (Channel 0)))
        "\xb0\x79\x00"
    , SC "LocalControl True"
        (ChannelMode (LocalControl (Channel 1) True))
        "\xb1\x7a\x7f"
    , SC "LocalControl False"
        (ChannelMode (LocalControl (Channel 1) False))
        "\xb1\x7a\x00"
    , SC "All Notes Off"
        (ChannelMode (AllNotesOff (Channel 0)))
        "\xb0\x7b\x00"
    , SC "Omni Off"
        (ChannelMode (OmniOff (Channel 0)))
        "\xb0\x7c\x00"
    , SC "Omni On"
        (ChannelMode (OmniOn (Channel 0)))
        "\xb0\x7d\x00"
    , SC "Mono On"
        (ChannelMode (MonoOn (Channel 0) (to7Bit 3)))
        "\xb0\x7e\x03"
    , SC "Poly On"
        (ChannelMode (PolyOn (Channel 0)))
        "\xb0\x7f\x00"
      -- System Common
    , SC "MTC Quarter"
        (SystemCommon (MTCQuarter (to7Bit 0)))
        "\xf1\x00"
    , SC "Song Position"
        (SystemCommon (SongPosition (mkPositionPointer 8)))
        "\xf2\x08\x00"
    , SC "Song Select"
        (SystemCommon (SongSelect (to7Bit 0)))
        "\xf3\x00"
    , SC "Tune Request"
        (SystemCommon TuneRequest)
        "\xf6"
    , SC "EOX"
        (SystemCommon EOX)
        "\xf7"
      -- System Real Time
    , SC "Timing Clock"
        (SystemRealTime TimingClock)
        "\xf8"
    , SC "Start"
        (SystemRealTime Start)
        "\xfa"
    , SC "Continue"
        (SystemRealTime Continue)
        "\xfb"
    , SC "Stop"
        (SystemRealTime Stop)
        "\xfc"
    , SC "Active Sensing"
        (SystemRealTime ActiveSensing)
        "\xfe"
    , SC "System Reset"
        (SystemRealTime SystemReset)
        "\xff"
      -- System Exclusive
    , SC "System Exclusive, short vendor"
        (SystemExclusive (Exclusive (VendorIdShort (to7Bit 8)) "000"))
        "\xf0\x08\x30\x30\x30\xf7"
    , SC "System Exclusive, long vendor"
        (SystemExclusive (Exclusive (VendorIdLong (to7Bit 8) (to7Bit 8)) "000"))
        "\xf0\x00\x08\x08\x30\x30\x30\xf7"
    ]

{- Arbitrary instances for MidiMessage -}
instance Arbitrary MidiMessage where
    arbitrary = oneof [ ChannelVoice    <$> arbitrary
                      , ChannelMode     <$> arbitrary
                      , SystemCommon    <$> arbitrary
                      , SystemRealTime  <$> arbitrary
                      , SystemExclusive <$> arbitrary ]

instance Arbitrary ChannelVoice where
    arbitrary = oneof [ NoteOff <$> arbitrary <*> arbitrary <*> arbitrary
                      , NoteOn <$> arbitrary <*> arbitrary <*> arbitrary
                      , Aftertouch <$> arbitrary <*> arbitrary <*> arbitrary
                      , ControlChange <$> arbitrary <*> arbitrary 
                                      <*> (to7Bit <$> (arbitrary :: Gen Word8))
                      , PatchChange <$> arbitrary <*> arbitrary
                      , ChannelPressure <$> arbitrary <*> arbitrary
                      , PitchBend <$> arbitrary <*> 
                            (to14Bit <$> (arbitrary :: Gen Word16)) ]

instance Arbitrary ChannelMode where
    arbitrary = oneof [ AllSoundOff <$> arbitrary
                      , ResetAllControllers <$> arbitrary
                      , LocalControl <$> arbitrary <*> arbitrary
                      , AllNotesOff <$> arbitrary
                      , OmniOff <$> arbitrary
                      , OmniOn <$> arbitrary
                      , MonoOn <$> arbitrary 
                               <*> (to7Bit <$> (arbitrary :: Gen Word8))
                      , PolyOn <$> arbitrary ]

instance Arbitrary SystemCommon where
    arbitrary = oneof [ MTCQuarter <$> (to7Bit <$> (arbitrary :: Gen Word8))
                      , SongPosition <$> arbitrary
                      , SongSelect <$> (to7Bit <$> (arbitrary :: Gen Word8))
                      , pure TuneRequest
                      , pure EOX ]

instance Arbitrary SystemExclusive where
    arbitrary = Exclusive <$> arbitrary <*> 
        (B.pack . map to7Bit . getNonEmpty 
            <$> (arbitrary :: Gen (NonEmptyList Word8)))

instance Arbitrary SystemRealTime where
    arbitrary = elements [ TimingClock, Start, Continue, Stop
                         , ActiveSensing, SystemReset ]

instance Arbitrary VendorId where
    arbitrary = oneof [ VendorIdShort <$> (to7Bit' <$> (arbitrary :: Gen Word8))
                      , VendorIdLong  <$> (to7Bit <$> (arbitrary :: Gen Word8))
                                      <*> (to7Bit' <$> (arbitrary :: Gen Word8))
                      ]
        where to7Bit' x = if to7Bit x == 0 then 1 else to7Bit x

instance Arbitrary Channel where
    arbitrary = mkChannel <$> (arbitrary :: Gen Word8)

instance Arbitrary Pitch where
    arbitrary = mkPitch <$> (arbitrary :: Gen Word8)

instance Arbitrary Velocity where
    arbitrary = mkVelocity <$> (arbitrary :: Gen Word8)

instance Arbitrary Touch where
    arbitrary = mkTouch <$> (arbitrary :: Gen Word8)

instance Arbitrary Controller where
    arbitrary = do
        x <- to7Bit <$> (arbitrary :: Gen Word8)
        let x' = if x >= 0x78 && x <= 0x7F
                 then 0x00
                 else x
        pure $ mkController x'

instance Arbitrary Patch where
    arbitrary = mkPatch <$> (arbitrary :: Gen Word8)

instance Arbitrary PositionPointer where
    arbitrary = mkPositionPointer <$> (arbitrary :: Gen Word16)
