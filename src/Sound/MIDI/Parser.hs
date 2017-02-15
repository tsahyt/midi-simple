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
midiMessage = go =<< peekWord8'
    where go x = case x .&. 0xF0 of
                     0xB0 -> ChannelMode <$> channelMode 
                         <|> ChannelVoice <$> channelVoice
                     0xF0 -> system x
                     _    -> ChannelVoice <$> channelVoice
          system x
              | x == 0xF0 = SystemExclusive <$> systemExclusive
              | x <= 0xF7 = SystemCommon <$> systemCommon
              | otherwise = SystemRealTime <$> systemRealTime

skipToStatus :: Parser ()
skipToStatus = skipWhile (not . flip testBit 7)

channelVoice :: Parser ChannelVoice
channelVoice = go =<< peekWord8'
    where go x = case x .&. 0xF0 of
                     0x80 -> noteOff
                     0x90 -> noteOn
                     0xA0 -> aftertouch
                     0xB0 -> controlChange
                     0xC0 -> patchChange
                     0xD0 -> channelPressure
                     0xE0 -> pitchBend
                     _    -> empty

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
channelMode = channelMessage 0x0B $ \c -> anyWord8 >>= \case
    0x78 -> AllSoundOff (Channel c) <$ word8 0x00
    0x79 -> ResetAllControllers (Channel c) <$ word8 0x00
    0x7A -> LocalControl (Channel c) <$> bool'
    0x7B -> AllNotesOff (Channel c) <$ word8 0x00
    0x7C -> OmniOff (Channel c) <$ word8 0x00
    0x7D -> OmniOn (Channel c) <$ word8 0x00
    0x7E -> MonoOn (Channel c) <$> anyWord8
    0x7F -> PolyOn (Channel c) <$ word8 0x00
    _    -> empty

    where bool' = anyWord8 >>= \case
                      0x00 -> pure False
                      0x7f -> pure True
                      _    -> empty

systemCommon :: Parser SystemCommon
systemCommon = peekWord8' >>= \case
    0xF1 -> mtcQuarter
    0xF2 -> songPosition
    0xF3 -> songSelect
    0xF6 -> tuneRequest
    0xF7 -> eox
    _    -> empty

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
systemRealTime = anyWord8 >>= \case
    0xF8 -> pure TimingClock 
    0xFA -> pure Start 
    0xFB -> pure Continue 
    0xFC -> pure Stop 
    0xFE -> pure ActiveSensing 
    0xFF -> pure SystemReset 
    _    -> empty

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
