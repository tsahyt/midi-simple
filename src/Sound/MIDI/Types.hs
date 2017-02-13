module Sound.MIDI.Types
(
    -- * Basic MIDI types
    ChannelVoice (..),

    -- * Numerical values
    Channel (..),
    Pitch (..),
    Patch (..),
    Velocity (..),
    Touch (..),
    Controller (..)
)
where

import Data.Word

data ChannelVoice
    = NoteOff Channel Pitch Velocity
    | NoteOn Channel Pitch Velocity
    | Aftertouch Channel Pitch Touch
    | ControlChange Channel Controller Word8
    | PatchChange Channel Patch
    | ChannelPressure Channel Touch
    | PitchBend Channel Word16
    deriving (Eq, Show, Ord, Read)

newtype Channel = Channel { getChannel :: Word8 }
    deriving (Eq, Show, Ord, Read)

newtype Pitch = Pitch { getPitch :: Word8 }
    deriving (Eq, Show, Ord, Read)

newtype Velocity = Velocity { getVelocity :: Word8 }
    deriving (Eq, Show, Ord, Read)

newtype Touch = Touch { getTouch :: Word8 }
    deriving (Eq, Show, Ord, Read)

newtype Controller = Controller { getController :: Word8 }
    deriving (Eq, Show, Ord, Read)

newtype Patch = Patch { getPatch :: Word8 }
    deriving (Eq, Show, Ord, Read)
