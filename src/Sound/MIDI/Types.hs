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

newtype Channel = Channel { getChannel :: Word8 }

newtype Pitch = Pitch { getPitch :: Word8 }

newtype Velocity = Velocity { getVelocity :: Word8 }

newtype Touch = Touch { getTouch :: Word8 }

newtype Controller = Controller { getController :: Word8 }

newtype Patch = Patch { getPatch :: Word8 }
