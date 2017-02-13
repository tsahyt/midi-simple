{-# LANGUAGE LambdaCase #-}
module Sound.MIDI.Serialize where

import Sound.MIDI.Types
import Data.ByteString.Builder
import Data.Monoid
import Data.Word
import Data.Bits

message :: ChannelVoice -> Builder
message = \case
    NoteOff c p v -> 
        channelStatus 0x80 c <> pitch p <> velocity v
    NoteOn c p v -> 
        channelStatus 0x90 c <> pitch p <> velocity v
    Aftertouch c p t -> 
        channelStatus 0xA0 c <> pitch p <> touch t
    ControlChange c n d -> 
        channelStatus 0xB0 c <> controller n <> word8 d
    PatchChange c p -> 
        channelStatus 0xC0 c <> patch p
    ChannelPressure c t -> 
        channelStatus 0xD0 c <> touch t
    PitchBend c v -> 
        channelStatus 0xE0 c <> pitchbend v

channelStatus :: Word8 -> Channel -> Builder
channelStatus p c = word8 $ p .|. getChannel c
{-# INLINE channelStatus #-}

pitchbend :: Word16 -> Builder
pitchbend v =
    let l = fromIntegral $ v .&. 0x0007
        m = fromIntegral $ v .&. 0x3f80
     in word8 l <> word8 m
{-# INLINE pitchbend #-}

pitch :: Pitch -> Builder
pitch = word8 . getPitch
{-# INLINE pitch #-}

patch :: Patch -> Builder
patch = word8 . getPatch
{-# INLINE patch #-}

velocity :: Velocity -> Builder
velocity = word8 . getVelocity
{-# INLINE velocity #-}

touch :: Touch -> Builder
touch = word8 . getTouch
{-# INLINE touch #-}

controller :: Controller -> Builder
controller = word8 . getController
{-# INLINE controller #-}
