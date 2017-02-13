module Sound.MIDI.Parser where

import Control.Applicative
import Sound.MIDI.Types
import Data.Bits
import Data.Word
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B

import Prelude hiding (take)

message :: Parser ChannelVoice
message = choice [ noteOff, noteOn, aftertouch, controlChange, patchChange
                 , channelPressure, pitchBend ]

channelMessage :: Word8 -> (Word8 -> Parser ChannelVoice) -> Parser ChannelVoice
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
    PitchBend (Channel c) <$> (go <$> take 2)
    where go x = let l = x `B.index` 0
                     m = x `B.index` 1
                  in unsafeShiftL (fromIntegral m) 7 + fromIntegral l

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
