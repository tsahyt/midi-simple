{-# LANGUAGE LambdaCase #-}
module Sound.MIDI.Serialize where

import Sound.MIDI.Types
import Data.ByteString.Builder
import Data.Monoid
import Data.Word
import Data.Bits

channelVoice :: ChannelVoice -> Builder
channelVoice = \case
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
        channelStatus 0xE0 c <> word14 v

channelStatus :: Word8 -> Channel -> Builder
channelStatus p c = word8 $ p .|. getChannel c
{-# INLINE channelStatus #-}

word14 :: Word16 -> Builder
word14 v =
    let l = fromIntegral $ v .&. 0x0007
        m = fromIntegral $ v .&. 0x3f80
     in word8 l <> word8 m
{-# INLINE word14 #-}

channelMode :: ChannelMode -> Builder
channelMode = \case
    AllSoundOff c ->
        channelStatus 0x0B c <> word8 0x78 <> word8 0x00
    ResetAllControllers c ->
        channelStatus 0x0B c <> word8 0x79 <> word8 0x00
    LocalControl c b ->
        channelStatus 0x0B c <> word8 0x7A <> bool' b
    AllNotesOff c ->
        channelStatus 0x0B c <> word8 0x7B <> word8 0x00
    OmniOff c ->
        channelStatus 0x0B c <> word8 0x7C <> word8 0x00
    OmniOn c ->
        channelStatus 0x0B c <> word8 0x7D <> word8 0x00
    MonoOn c n ->
        channelStatus 0x0B c <> word8 0x7E <> word8 n
    PolyOn c ->
        channelStatus 0x0B c <> word8 0x7F <> word8 0x00

    where bool' True  = word8 0x7F
          bool' False = word8 0x00

systemCommon :: SystemCommon -> Builder
systemCommon = \case
    MTCQuarter v -> word8 0xF1 <> word8 v
    SongPosition pp -> word8 0xF2 <> word14 (getPositionPointer pp)
    SongSelect x -> word8 0xF3 <> word8 x
    TuneRequest -> word8 0xF6
    EOX -> word8 0xF7

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
