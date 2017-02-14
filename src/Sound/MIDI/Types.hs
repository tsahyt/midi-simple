{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sound.MIDI.Types
(
    -- * Basic MIDI types
    ChannelVoice (..),
    ChannelMode (..),
    SystemCommon (..),
    SystemRealTime (..),

    -- * Numeric MIDI data
    --
    -- | Only use the direct constructors when you can assure that the values
    -- fit into 7 bits! In general you should prefer the smart constructors. For
    -- values outside of the 7 bit range, the numbers should generally wrap
    -- around, but no guarantees are made!
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
    mkController,
    PositionPointer (..),
    mkPositionPointer,
    toClocks
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

-- | A type for channel mode messages. Mode messages determine how an instrument
-- will receive all subsequent voice messages. This includes whether the
-- receiver will play notes monophonically or polyphonically and whether it will
-- respond only to data sent on one specific voice channel or all of them.
data ChannelMode
    = AllSoundOff !Channel
    | ResetAllControllers !Channel
    | LocalControl !Channel !Bool
    | AllNotesOff !Channel
    | OmniOff !Channel
    | OmniOn !Channel
    | MonoOn !Channel !Word8
    | PolyOn !Channel
    deriving (Eq, Show, Ord, Read, Generic)

data SystemCommon
    = MTCQuarter !Word8
    | SongPosition !PositionPointer
    | SongSelect !Word8
    | TuneRequest
    | EOX
    deriving (Eq, Show, Ord, Read, Generic)

data SystemRealTime
    = TimingClock
    | Start
    | Continue
    | Stop
    | ActiveSensing
    | SystemReset
    deriving (Eq, Show, Ord, Read, Generic)

to7Bit :: Integral a => a -> Word8
to7Bit = (.&. 0x7F) . fromIntegral

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

newtype PositionPointer = PositionPointer { getPositionPointer :: Word16 }
    deriving (Eq, Show, Ord, Read)

to14Bit :: Integral a => a -> Word16
to14Bit = (.&. 0x3FFF) . fromIntegral

mkPositionPointer :: Integral a => a -> PositionPointer
mkPositionPointer = PositionPointer . to14Bit

-- | Convert a 'PositionPointer', used to indicate song position, to the MIDI
-- clock. Song Position Pointer is always multiplied by 6 times the MIDI clocks
-- (F8H). Thus the smallest Song Position change is 6 MIDI clocks, or 1/16 note.
toClocks :: PositionPointer -> Int
toClocks = (* 6) . fromIntegral . getPositionPointer
{-# INLINEABLE toClocks #-}
