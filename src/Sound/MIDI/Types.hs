{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sound.MIDI.Types
(
    -- * Basic MIDI types
    ChannelVoice (..),

    -- * Numeric MIDI data
    --
    -- Only use the direct constructors when you can assure that the values fit
    -- into 7 bits! In general you should prefer the smart constructors.
    Channel (..),
    mkChannel,
    Pitch (..),
    mkPitch,
    Patch (..),
    mkPatch,
    Velocity (..),
    mkVelocity,
    Touch (..),
    mkTouch,
    Controller (..),
    mkController
)
where

import Data.Word
import Data.Bits
import GHC.Generics

-- | Type holding channel voice messages. Channel Voice messages transmit
-- real-time performance data over a single channel. Examples include "note-on"
-- messages which contain a MIDI note number that specifies the note's pitch, a
-- velocity value that indicates how forcefully the note was played, and the
-- channel number; "note-off" messages that end a note; program change messages
-- that change a device's patch; and control changes that allow adjustment of an
-- instrument's parameters.
data ChannelVoice
    = NoteOff !Channel !Pitch !Velocity
    | NoteOn !Channel !Pitch !Velocity
    | Aftertouch !Channel !Pitch !Touch
    | ControlChange !Channel !Controller !Word8
    | PatchChange !Channel !Patch
    | ChannelPressure !Channel !Touch
    | PitchBend !Channel !Word16
    deriving (Eq, Show, Ord, Read, Generic)

to7Bit :: Integral a => a -> Word8
to7Bit = (.&. 0xEF) . fromIntegral

newtype Channel = Channel { getChannel :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkChannel :: Integral a => a -> Channel
mkChannel = Channel . to7Bit

newtype Pitch = Pitch { getPitch :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkPitch :: Integral a => a -> Pitch 
mkPitch = Pitch . to7Bit

newtype Velocity = Velocity { getVelocity :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkVelocity :: Integral a => a -> Velocity 
mkVelocity = Velocity . to7Bit

newtype Touch = Touch { getTouch :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkTouch :: Integral a => a -> Touch 
mkTouch = Touch . to7Bit

newtype Controller = Controller { getController :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkController :: Integral a => a -> Controller 
mkController = Controller . to7Bit

newtype Patch = Patch { getPatch :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkPatch :: Integral a => a -> Patch 
mkPatch = Patch . to7Bit
