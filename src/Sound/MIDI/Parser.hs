{-# LANGUAGE LambdaCase #-}
-- | Parsers for 'MidiMessage' and its components, implemented as Attoparsec
-- parsers. See "Data.Attoparsec.ByteString" for how to run them. In most common
-- use cases, the 'decodeMidi' function in "Sound.MIDI" should suffice.
module Sound.MIDI.Parser where

import Control.Applicative
import Sound.MIDI.Types
import Data.Bits
import Data.Word
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B

import Prelude hiding (take)

midiMessage :: Parser MidiMessage
midiMessage = choice 
    [ ChannelMode <$> channelMode
    , ChannelVoice <$> channelVoice
    , SystemCommon <$> systemCommon
    , SystemRealTime <$> systemRealTime
    , SystemExclusive <$> systemExclusive ]

skipToStatus :: Parser ()
skipToStatus = skipWhile (not . flip testBit 7)

channelVoice :: Parser ChannelVoice
channelVoice = choice [ noteOff, noteOn, aftertouch, controlChange, patchChange
                      , channelPressure, pitchBend ]

channelMessage :: Word8 -> (Word8 -> Parser a) -> Parser a
channelMessage header p = do
    status <- anyWord8
    let upper = unsafeShiftR status 4
        lower = status .&. 0x0F
    if upper == header then p lower else empty
{-# INLINE channelMessage #-}

noteOff :: Parser ChannelVoice
noteOff = channelMessage 0x08 $ \c ->
    NoteOff (Channel c) <$> pitch <*> velocity

noteOn :: Parser ChannelVoice
noteOn = channelMessage 0x09 $ \c ->
    NoteOn (Channel c) <$> pitch <*> velocity

aftertouch :: Parser ChannelVoice
aftertouch = channelMessage 0x0A $ \c ->
    Aftertouch (Channel c) <$> pitch <*> touch

controlChange :: Parser ChannelVoice
controlChange = channelMessage 0x0B $ \c ->
    ControlChange (Channel c) <$> controller <*> anyWord8

patchChange :: Parser ChannelVoice
patchChange = channelMessage 0x0C $ \c ->
    PatchChange (Channel c) <$> patch

channelPressure :: Parser ChannelVoice
channelPressure = channelMessage 0x0D $ \c ->
    ChannelPressure (Channel c) <$> touch

pitchBend :: Parser ChannelVoice
pitchBend = channelMessage 0x0E $ \c ->
    PitchBend (Channel c) <$> anyWord14

anyWord14 :: Parser Word16
anyWord14 = go <$> take 2
    where go x = let l = x `B.index` 0
                     m = x `B.index` 1
                  in unsafeShiftL (fromIntegral m) 7 + fromIntegral l

channelMode :: Parser ChannelMode
channelMode = choice [ allSoundOff, resetAllControllers, localControl
                     , allNotesOff, omniOff, omniOn, monoOn, polyOn ]

allSoundOff :: Parser ChannelMode
allSoundOff = channelMessage 0x0B $ \c ->
    AllSoundOff (Channel c) <$ word8 0x78 <* word8 0x00

resetAllControllers :: Parser ChannelMode
resetAllControllers = channelMessage 0x0B $ \c ->
    ResetAllControllers (Channel c) <$ word8 0x79 <* word8 0x00

localControl :: Parser ChannelMode
localControl = channelMessage 0x0B $ \c ->
    LocalControl (Channel c) <$> (word8 0x7A *> bool')
    where bool' = anyWord8 >>= \case
                      0x00 -> pure False
                      0x7f -> pure True
                      _    -> empty

allNotesOff :: Parser ChannelMode
allNotesOff = channelMessage 0x0B $ \c ->
    AllNotesOff (Channel c) <$ word8 0x7B <* word8 0x00

omniOff :: Parser ChannelMode
omniOff = channelMessage 0x0B $ \c ->
    OmniOff (Channel c) <$ word8 0x7C <* word8 0x00

omniOn :: Parser ChannelMode
omniOn = channelMessage 0x0B $ \c ->
    OmniOn (Channel c) <$ word8 0x7D <* word8 0x00

monoOn :: Parser ChannelMode
monoOn = channelMessage 0x0B $ \c ->
    MonoOn (Channel c) <$> (word8 0x7E *> anyWord8)

polyOn :: Parser ChannelMode
polyOn = channelMessage 0x0B $ \c ->
    PolyOn (Channel c) <$ word8 0x7F <* word8 0x00

systemCommon :: Parser SystemCommon
systemCommon = choice [ mtcQuarter, songPosition, songSelect, tuneRequest
                      , eox ]

mtcQuarter :: Parser SystemCommon
mtcQuarter = MTCQuarter <$> (word8 0xF1 *> anyWord8) 

songPosition :: Parser SystemCommon
songPosition = SongPosition <$> (word8 0xF2 *> (PositionPointer <$> anyWord14))

songSelect :: Parser SystemCommon
songSelect = SongSelect <$> (word8 0xF3 *> anyWord8)

tuneRequest :: Parser SystemCommon
tuneRequest = TuneRequest <$ word8 0xF6

eox :: Parser SystemCommon
eox = EOX <$ word8 0xF7

systemRealTime :: Parser SystemRealTime
systemRealTime = choice
    [ TimingClock <$ word8 0xF8
    , Start <$ word8 0xFA
    , Continue <$ word8 0xFB
    , Stop <$ word8 0xFC
    , ActiveSensing <$ word8 0xFE
    , SystemReset <$ word8 0xFF ]

systemExclusive :: Parser SystemExclusive
systemExclusive = Exclusive
    <$> (word8 0xF0 *> vendorId) 
    <*> takeTill (`testBit` 7)

vendorId :: Parser VendorId
vendorId = longId <|> shortId
    where longId  = VendorIdLong  <$> (word8 0x00 *> anyWord8) <*> anyWord8
          shortId = VendorIdShort <$> anyWord8

-- | Parse a 'Pitch', no check for bit 7 is performed!
pitch :: Parser Pitch
pitch = Pitch <$> anyWord8

-- | Parse a 'Pitch', no check for bit 7 is performed!
patch :: Parser Patch
patch = Patch <$> anyWord8

-- | Parse a 'Velocity', no check for bit 7 is performed!
velocity :: Parser Velocity
velocity = Velocity <$> anyWord8

-- | Parse a 'Touch', no check for bit 7 is performed!
touch :: Parser Touch
touch = Touch <$> anyWord8

-- | Parse a 'Controller', no check for bit 7 is performed!
controller :: Parser Controller
controller = Controller <$> anyWord8
