{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString as B
import Data.Word
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Sound.MIDI
import Sound.MIDI.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testProperty "parse . serialize == id" $
    \(msg :: MidiMessage) ->
        decodeMidi1 (encodeMidi1' msg) == Right msg

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
        (B.pack . map to7Bit <$> (arbitrary :: Gen [Word8]))

instance Arbitrary SystemRealTime where
    arbitrary = elements [ TimingClock, Start, Continue, Stop
                         , ActiveSensing, SystemReset ]

instance Arbitrary VendorId where
    arbitrary = oneof [ VendorIdShort <$> (to7Bit <$> (arbitrary :: Gen Word8))
                      , VendorIdLong  <$> (to7Bit <$> (arbitrary :: Gen Word8))
                                      <*> (to7Bit <$> (arbitrary :: Gen Word8))
                      ]

instance Arbitrary Channel where
    arbitrary = mkChannel <$> (arbitrary :: Gen Word8)

instance Arbitrary Pitch where
    arbitrary = mkPitch <$> (arbitrary :: Gen Word8)

instance Arbitrary Velocity where
    arbitrary = mkVelocity <$> (arbitrary :: Gen Word8)

instance Arbitrary Touch where
    arbitrary = mkTouch <$> (arbitrary :: Gen Word8)

instance Arbitrary Controller where
    arbitrary = mkController <$> (arbitrary :: Gen Word8)

instance Arbitrary Patch where
    arbitrary = mkPatch <$> (arbitrary :: Gen Word8)

instance Arbitrary PositionPointer where
    arbitrary = mkPositionPointer <$> (arbitrary :: Gen Word16)
